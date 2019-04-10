CREATE OR REPLACE FUNCTION test_pipeline_user_roles_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('pipeline_user_roles');

-- Columns
  RETURN QUERY SELECT columns_are('pipeline_user_roles', ARRAY['pipeline_id',
                                                               'user_id',
                                                               'role'
                                                               ]);

  RETURN QUERY SELECT col_is_pk('pipeline_user_roles', ARRAY['pipeline_id','user_id','role']);

  RETURN QUERY SELECT col_not_null('pipeline_user_roles', 'pipeline_id');
  RETURN QUERY SELECT col_not_null('pipeline_user_roles', 'user_id');
  RETURN QUERY SELECT col_not_null('pipeline_user_roles', 'role');

END;
$$;
