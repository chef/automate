CREATE OR REPLACE FUNCTION test_user_cache_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('user_cache');

-- Columns
  RETURN QUERY SELECT columns_are('user_cache', ARRAY['id','cached_at']);

  RETURN QUERY SELECT col_is_pk('user_cache', 'id');

END;
$$;
