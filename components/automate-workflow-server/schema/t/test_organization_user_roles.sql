CREATE OR REPLACE FUNCTION test_organization_user_roles_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('organization_user_roles');

-- Columns
  RETURN QUERY SELECT columns_are('organization_user_roles', ARRAY['organization_id',
                                                                 'user_id',
                                                                 'role'
                                                                 ]);

  RETURN QUERY SELECT col_is_pk('organization_user_roles', ARRAY['organization_id','user_id','role']);

  RETURN QUERY SELECT col_not_null('organization_user_roles', 'organization_id');
  RETURN QUERY SELECT col_not_null('organization_user_roles', 'user_id');
  RETURN QUERY SELECT col_not_null('organization_user_roles', 'role');


END;
$$;
