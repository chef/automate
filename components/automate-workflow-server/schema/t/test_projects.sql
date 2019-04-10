CREATE OR REPLACE FUNCTION test_projects_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('projects');

  -- Columns
  RETURN QUERY SELECT columns_are('projects', ARRAY['id',
                                                     'organization_id',
                                                     'guid',
                                                     'name',
                                                     'scm_module'
                                                    ]);

  RETURN QUERY SELECT col_is_pk('projects', 'id');
  RETURN QUERY SELECT col_is_unique('projects', ARRAY['organization_id', 'name']);
  RETURN QUERY SELECT col_not_null('projects', 'guid');
  RETURN QUERY SELECT col_is_unique('projects', 'guid');
END;
$$;
