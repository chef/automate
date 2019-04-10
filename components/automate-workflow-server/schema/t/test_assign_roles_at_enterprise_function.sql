CREATE OR REPLACE FUNCTION test_assign_roles_at_enterprise_function()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
DECLARE
  test_enterprise CONSTANT enterprises.name%TYPE = 'BigCo';
  test_user CONSTANT users.name%TYPE  = 'BigCo User';
BEGIN
  RETURN QUERY SELECT lives_ok(
    format('SELECT assign_roles_at_enterprise(%L, %L, %L)', test_enterprise, test_user, ARRAY['admin', 'committer']),
    'Can assign roles to a user at the enterprise level');

  RETURN QUERY SELECT set_eq(
    format('SELECT role FROM enterprise_user_roles WHERE enterprise_id = %L AND user_id = %L', ent(test_enterprise), u(test_enterprise, test_user)),
    ARRAY['admin', 'committer']::delivery_role[],
    'Expected roles are assigned to a user at the enterprise level'
  );

  RETURN QUERY SELECT lives_ok(
    format('SELECT assign_roles_at_enterprise(%L, %L, %L)', test_enterprise, test_user, ARRAY['admin', 'observer']),
    'Can re-assign all roles to a user at the enterprise level');

  RETURN QUERY SELECT set_eq(
    format('SELECT role FROM enterprise_user_roles WHERE enterprise_id = %L AND user_id = %L', ent(test_enterprise), u(test_enterprise, test_user)),
    ARRAY['admin', 'observer']::delivery_role[],
    'All roles have been re-set for a user at the enterprise level'
  );

  RETURN QUERY SELECT lives_ok(
    format('SELECT assign_roles_at_enterprise(%L, %L, %L)', test_enterprise, test_user, ARRAY[]::text[]),
    'Can remove all roles from a user at the enterprise level');

  RETURN QUERY SELECT set_eq(
    format('SELECT role FROM enterprise_user_roles WHERE enterprise_id = %L AND user_id = %L', ent(test_enterprise), u(test_enterprise, test_user)),
    ARRAY[]::delivery_role[],
    'All roles have been removed for a user at the enterprise level'
  );

END;
$$;
