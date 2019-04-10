CREATE OR REPLACE FUNCTION test_invalidate_everything()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
DECLARE
  enterprise_1_id CONSTANT enterprises.id%TYPE = ent('BigCo');
  enterprise_2_id CONSTANT enterprises.id%TYPE = ent('SmallCo');

  user_1_id CONSTANT users.id%TYPE = u('BigCo', 'BigCo User');
  user_2_id CONSTANT users.id%TYPE = u('SmallCo', 'SmallCo User');

  user_1_token CONSTANT user_tokens.auth_token%TYPE = 'fake_bigco_token';
  user_2_token CONSTANT user_tokens.auth_token%TYPE = 'fake_smallco_token';

BEGIN
  RETURN QUERY SELECT set_has(
    'SELECT id, auth_token FROM user_tokens',
    format('VALUES (%s, %L), (%s, %L)', user_1_id, user_1_token, user_2_id, user_2_token),
    'Tokens are present before invalidating');

  PERFORM invalidate('token');

  RETURN QUERY SELECT results_eq(
    'SELECT id, auth_token FROM user_tokens',
    -- fake an empty result set
    'SELECT id, auth_token FROM user_tokens WHERE FALSE',
    'Everyone''s tokens disappeared!');

  INSERT INTO user_passwords(id, hashed_pass, hash_type)
  VALUES (user_1_id, 'bigcouserpasswordhash', 'bcrypt'),
         (user_2_id, 'smallcouserpasswordhash', 'bcrypt');

  RETURN QUERY SELECT set_has(
    'SELECT id FROM user_passwords',
    format('VALUES (%s), (%s)', user_1_id, user_2_id),
    'Passwords are present before invalidating');

  PERFORM invalidate('password');

  RETURN QUERY SELECT results_eq(
    'SELECT id FROM user_passwords',
    'SELECT id FROM user_passwords WHERE FALSE',
    'Everyone''s passwords disappeared!'
  );
END;
$$;
