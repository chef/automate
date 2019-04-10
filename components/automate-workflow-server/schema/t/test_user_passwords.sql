CREATE OR REPLACE FUNCTION test_user_passwords_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('user_passwords');

-- Columns
  RETURN QUERY SELECT columns_are('user_passwords', ARRAY['id',
                                                          'hashed_pass',
                                                          'hash_type']);

  RETURN QUERY SELECT col_is_pk('user_passwords', 'id');

END;
$$;
