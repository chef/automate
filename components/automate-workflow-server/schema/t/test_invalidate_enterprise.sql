CREATE OR REPLACE FUNCTION test_invalidate_enterprise()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
DECLARE
  enterprise_1_id CONSTANT enterprises.id%TYPE = ent('BigCo');
  enterprise_2_id CONSTANT enterprises.id%TYPE = ent('SmallCo');

  user_1_id CONSTANT users.id%TYPE = u('BigCo', 'BigCo User');
  user_2_id CONSTANT users.id%TYPE = u('BigCo', 'BigCo Chaos Monkey');
  user_3_id CONSTANT users.id%TYPE = u('SmallCo', 'SmallCo User');

  user_1_token CONSTANT user_tokens.auth_token%TYPE = 'fake_bigco_token';
  user_2_token CONSTANT user_tokens.auth_token%TYPE = 'big_chaos_monkey_token';
  user_3_token CONSTANT user_tokens.auth_token%TYPE = 'fake_smallco_token';

BEGIN
  RETURN QUERY SELECT set_has(
    'SELECT id, auth_token FROM user_tokens',
    format('VALUES (%s, %L), (%s, %L), (%s, %L)', user_1_id, user_1_token, user_2_id, user_2_token, user_3_id, user_3_token),
    'Tokens are present before invalidating');

  PERFORM invalidate('token', 'BigCo');

  RETURN QUERY SELECT set_hasnt(
    'SELECT id, auth_token FROM user_tokens',
    format('VALUES (%s, %L), (%s, %L)', user_1_id, user_1_token, user_2_id, user_2_token),
    'BigCo''s tokens disappeared!');

  RETURN QUERY SELECT set_has(
    'SELECT id, auth_token FROM user_tokens',
    format('VALUES (%s, %L)', user_3_id, user_3_token),
    'SmallCo''s tokens are still there!');

  INSERT INTO user_passwords(id, hashed_pass, hash_type)
  VALUES (user_1_id, 'bigcouserpasswordhash', 'bcrypt'),
         (user_2_id, 'bigcomonkeypasswordhash', 'bcrypt'),
         (user_3_id, 'smallcouserpasswordhash', 'bcrypt');

  RETURN QUERY SELECT set_has(
    'SELECT id FROM user_passwords',
    format('VALUES (%s), (%s), (%s)', user_1_id, user_2_id, user_3_id),
    'Passwords are present before invalidating');

  PERFORM invalidate('password', 'BigCo');

  RETURN QUERY SELECT set_hasnt(
    'SELECT id FROM user_passwords',
    format('VALUES (%s), (%s)', user_1_id, user_2_id),
    'BigCo''s passwords disappeared!'
  );

  RETURN QUERY SELECT set_has(
    'SELECT id FROM user_passwords',
    format('VALUES (%s)', user_3_id),
    'SmallCo''s passwords are still there!'
  );

END;
$$;
