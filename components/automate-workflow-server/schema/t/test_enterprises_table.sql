CREATE OR REPLACE FUNCTION test_enterprises_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('enterprises');

-- Columns
  RETURN QUERY SELECT columns_are('enterprises', ARRAY['id',
                                                       'name']);

  RETURN QUERY SELECT col_is_pk('enterprises', 'id');
  RETURN QUERY SELECT col_is_unique('enterprises', 'name');

END;
$$;
