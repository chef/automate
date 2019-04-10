CREATE OR REPLACE FUNCTION test_user_tokens_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('user_tokens');

-- Columns
  RETURN QUERY SELECT columns_are('user_tokens', ARRAY['id',
                                                       'auth_token',
                                                       'auth_token_bday']);

  RETURN QUERY SELECT col_is_pk('user_tokens', ARRAY['id','auth_token']);

END;
$$;
