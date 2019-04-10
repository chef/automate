CREATE OR REPLACE FUNCTION test_project_user_roles_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('project_user_roles');

  -- Columns
  RETURN QUERY SELECT columns_are('project_user_roles', ARRAY['project_id',
                                                              'user_id',
                                                              'role'
                                                              ]);

  RETURN QUERY SELECT col_is_pk('project_user_roles', ARRAY['project_id','user_id','role']);

  RETURN QUERY SELECT col_not_null('project_user_roles', 'project_id');
  RETURN QUERY SELECT col_not_null('project_user_roles', 'user_id');
  RETURN QUERY SELECT col_not_null('project_user_roles', 'role');


END;
$$;
