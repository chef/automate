CREATE OR REPLACE FUNCTION test_organizations_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('organizations');

-- Columns
  RETURN QUERY SELECT columns_are('organizations', ARRAY['id',
                                                         'enterprise_id',
                                                         'name']);

  RETURN QUERY SELECT col_is_pk('organizations', 'id');
  RETURN QUERY SELECT col_is_unique('organizations', ARRAY['enterprise_id','name']);

END;
$$;
