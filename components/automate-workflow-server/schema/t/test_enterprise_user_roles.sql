CREATE OR REPLACE FUNCTION test_enterprise_user_roles_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('enterprise_user_roles');

-- Columns
  RETURN QUERY SELECT columns_are('enterprise_user_roles', ARRAY['enterprise_id',
                                                                 'user_id',
                                                                 'role'
                                                                 ]);

  RETURN QUERY SELECT col_is_pk('enterprise_user_roles', ARRAY['enterprise_id','user_id','role']);

  RETURN QUERY SELECT col_not_null('enterprise_user_roles', 'enterprise_id');
  RETURN QUERY SELECT col_not_null('enterprise_user_roles', 'user_id');
  RETURN QUERY SELECT col_not_null('enterprise_user_roles', 'role');

END;
$$;
