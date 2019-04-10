CREATE OR REPLACE FUNCTION test_revoke_token()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
DECLARE
  enterprise_1_id CONSTANT enterprises.id%TYPE = ent('BigCo');
  user_1_id CONSTANT users.id%TYPE = u('BigCo', 'BigCo User');
  user_1_token CONSTANT user_tokens.auth_token%TYPE = 'fake_bigco_token';
BEGIN

  INSERT INTO user_tokens(id, auth_token)
  VALUES (user_1_id, 'this_is_a_new_token');

  RETURN QUERY SELECT is(count(*), 2::bigint, 'User has 2 tokens')
    FROM user_tokens
    WHERE id = user_1_id;

  RETURN QUERY SELECT is(
    revoke_token('BigCo', 'BigCo User', 'this_is_a_new_token'),
    'this_is_a_new_token',
    'revoke_token returns the token it revoked');

  RETURN QUERY SELECT results_eq(
    format('SELECT auth_token FROM user_tokens WHERE id = %s', user_1_id),
    ARRAY[user_1_token],
    'Newly-added token should have been removed'
  );

  RETURN QUERY SELECT is(
    revoke_token('BigCo', 'BigCo User', 'this_is_a_new_token'),
    NULL,
    'revoke_token returns NULL if the token doesn''t exist');

  RETURN QUERY SELECT is(
    revoke_token('BigCo', 'who_the_hell_is_this', 'this_is_a_new_token'),
    NULL,
    'revoke_token returns NULL if the user doesn''t exist');

  RETURN QUERY SELECT is(
    revoke_token('Eduardo''s Carne Asada Emporium', 'BigCo User', 'this_is_a_new_token'),
    NULL,
    'revoke_token returns NULL if the enterprise doesn''t exist');

END;
$$;
