CREATE OR REPLACE FUNCTION test_candidate_tokens_function()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
DECLARE
  test_user_id CONSTANT users.id%TYPE = u('BigCo', 'BigCo User');
  test_token CONSTANT user_tokens.auth_token%TYPE     = 'fake_bigco_token';

  bad_test_token CONSTANT user_tokens.auth_token%TYPE = 'fake_bigco_token_is_fake';

  birthday cd_timestamp DEFAULT 'epoch';
  birthday_2 cd_timestamp DEFAULT 'epoch';

BEGIN
   -- just to facilitate testing with pgTAP's row_eq function (see
   -- http://pgtap.org/documentation.html#row_eq)
   CREATE TYPE candidate_token_type AS (token TEXT, birthday cd_timestamp);

   SELECT auth_token_bday
   FROM user_tokens
   WHERE id = test_user_id
     AND auth_token = test_token
   INTO birthday;

   RETURN QUERY SELECT row_eq(
     format('SELECT * FROM candidate_tokens(%L, %L, %L)', 'BigCo', 'BigCo User', test_token),
      ROW(test_token,birthday)::candidate_token_type,
     '"candidate_tokens" retrieves the token with the same prefix, as well as its birthday');

   RETURN QUERY SELECT row_eq(
     format('SELECT * FROM candidate_tokens(%L, %L, %L)', 'BigCo', 'BigCo User', bad_test_token),
     ROW(test_token,birthday)::candidate_token_type,
     '"candidate_tokens" retrieves the token with the same prefix; it doesn''t simply return the argument that was passed');

   -- Cannot cast NULL to the candidate_token_type because cd_timestamp doesn't allow NULL... hrmmm.
   RETURN QUERY SELECT results_eq(
     format('SELECT * FROM candidate_tokens(%L, %L, %L)', 'BigCo', 'BigCo User', 'such_token_very_secure_much_authentication_wow'),
     'SELECT auth_token, auth_token_bday FROM user_tokens WHERE FALSE',
     '"candidate_tokens" returns nothing if no tokens match the prefix of the given token');

   INSERT INTO user_tokens(id, auth_token)
   VALUES (test_user_id, bad_test_token)
   RETURNING auth_token_bday INTO birthday_2;

   RETURN QUERY SELECT results_eq(
     format('SELECT * FROM candidate_tokens(%L, %L, %L) ORDER by token', 'BigCo', 'BigCo User', test_token),
     format('SELECT auth_token, auth_token_bday FROM user_tokens WHERE id = %s AND auth_token::token_prefix = %L ORDER BY auth_token',
            test_user_id,
            test_token::token_prefix),
     '"candidate_tokens" returns all tokens that match the prefix');

END;
$$;
