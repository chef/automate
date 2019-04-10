CREATE OR REPLACE FUNCTION test_effective_roles_at_enterprise_function()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
DECLARE

BEGIN
  RETURN QUERY SELECT set_eq(
    format('SELECT effective_roles_at_enterprise(%L, %L)', 'BigCo', 'BigCo User'),
    ARRAY[]::delivery_role[],
    'BigCo User has no roles at the enterprise scope'
  );

  INSERT INTO enterprise_user_roles(enterprise_id, user_id, role)
  VALUES (ent('BigCo'), u('BigCo', 'BigCo User'), 'admin');

  RETURN QUERY SELECT set_eq(
    format('SELECT effective_roles_at_enterprise(%L, %L)', 'BigCo', 'BigCo User'),
    ARRAY['admin']::delivery_role[],
   'After being granted the admin role, BigCo User has "admin" as an effective role'
  );

  INSERT INTO enterprise_user_roles(enterprise_id, user_id, role)
  VALUES (ent('BigCo'), u('BigCo', 'BigCo User'), 'committer'),
         (ent('BigCo'), u('BigCo', 'BigCo User'), 'reviewer');

  RETURN QUERY SELECT set_eq(
    format('SELECT effective_roles_at_enterprise(%L, %L)', 'BigCo', 'BigCo User'),
    ARRAY['admin', 'committer', 'reviewer']::delivery_role[],
    'User has multiple granted roles at the enterprise scope'
  );

END;
$$;
