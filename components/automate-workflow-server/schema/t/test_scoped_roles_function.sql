CREATE OR REPLACE FUNCTION test_scoped_roles_function()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  PREPARE enterprise_scoped_roles(text, text) AS
    SELECT role, scopes FROM scoped_roles($1, $2, NULL, NULL, NULL)
    ORDER BY role;

  -- Slightly hacky way to get an empty result set of the proper
  -- form... ugh
  PREPARE no_permissions AS
    SELECT role, ARRAY[]::delivery_scope[]
    FROM enterprise_user_roles
    WHERE FALSE;

  PREPARE admin_only AS
    VALUES('admin'::delivery_role, ARRAY['enterprise']);

  -- Remember, these are ordered by *enum order*, NOT alphabetically!
  PREPARE enterprise_permissions AS
    VALUES ('admin'::delivery_role, ARRAY['enterprise']),
           ('committer'::delivery_role, ARRAY['enterprise']),
           ('reviewer'::delivery_role, ARRAY['enterprise']),
           ('shipper'::delivery_role, ARRAY['enterprise']),
           ('observer'::delivery_role, ARRAY['enterprise']);

  -- Enterprise Scope

  RETURN QUERY SELECT results_eq(
    format('EXECUTE enterprise_scoped_roles(%L, %L)', 'BigCo', 'BigCo User'),
    'no_permissions',
    'BigCo User has no permissions set at enterprise scope'
  );

  INSERT INTO enterprise_user_roles(enterprise_id, user_id, role)
  VALUES (ent('BigCo'), u('BigCo', 'BigCo User'), 'admin');

  RETURN QUERY SELECT results_eq(
    format('EXECUTE enterprise_scoped_roles(%L, %L)', 'BigCo', 'BigCo User'),
    'admin_only',
    'BigCo User has the admin role set at enterprise scope'
  );

  INSERT INTO enterprise_user_roles(enterprise_id, user_id, role)
  SELECT ent('BigCo'), u('BigCo', 'BigCo User'), role
  FROM unnest(ARRAY['committer', 'observer', 'reviewer', 'shipper']::delivery_role[]) AS roles(role);

  RETURN QUERY SELECT results_eq(
    format('EXECUTE enterprise_scoped_roles(%L, %L)', 'BigCo', 'BigCo User'),
    'enterprise_permissions',
    'BigCo User has all permissions set at enterprise scope'
  );

  -- Organization Scope
  PREPARE organization_scoped_roles(text, text, text) AS
    SELECT role, scopes FROM scoped_roles($1, $2, $3, NULL, NULL)
    ORDER BY role;

  RETURN QUERY SELECT results_eq(
    format('EXECUTE organization_scoped_roles(%L, %L, %L)', 'BigCo', 'BigCo User', 'BigCo Engineering'),
    'enterprise_permissions',
    'BigCo User still has its enterprise-scoped roles at the org scope'
  );

  INSERT INTO organization_user_roles(organization_id, user_id, role)
  SELECT org('BigCo', 'BigCo Engineering'), u('BigCo', 'BigCo User'), role
  FROM unnest(ARRAY['committer']::delivery_role[]) AS roles(role);

  PREPARE org_permissions AS
    VALUES ('admin'::delivery_role, ARRAY['enterprise']),
           ('committer'::delivery_role, ARRAY['enterprise', 'organization']),
           ('reviewer'::delivery_role, ARRAY['enterprise']),
           ('shipper'::delivery_role, ARRAY['enterprise']),
           ('observer'::delivery_role, ARRAY['enterprise']);

  RETURN QUERY SELECT results_eq(
    format('EXECUTE organization_scoped_roles(%L, %L, %L)', 'BigCo', 'BigCo User', 'BigCo Engineering'),
    'org_permissions',
    'BigCo User also has "committer" at the org scope'
  );

  -- Project Scope

  PREPARE project_scoped_roles(text, text, text, text) AS
    SELECT role, scopes FROM scoped_roles($1, $2, $3, $4, NULL)
    ORDER BY role;

  RETURN QUERY SELECT results_eq(
    format('EXECUTE project_scoped_roles(%L, %L, %L, %L)', 'BigCo', 'BigCo User', 'BigCo Engineering', 'skunkworks'),
    'org_permissions',
    'BigCo User still has its org-scoped roles at the project scope'
  );

  INSERT INTO project_user_roles(project_id, user_id, role)
  SELECT proj('BigCo', 'BigCo Engineering', 'skunkworks'), u('BigCo', 'BigCo User'), role
  FROM unnest(ARRAY['reviewer']::delivery_role[]) AS roles(role);

  PREPARE proj_permissions AS
    VALUES ('admin'::delivery_role, ARRAY['enterprise']),
           ('committer'::delivery_role, ARRAY['enterprise', 'organization']),
           ('reviewer'::delivery_role, ARRAY['enterprise', 'project']),
           ('shipper'::delivery_role, ARRAY['enterprise']),
           ('observer'::delivery_role, ARRAY['enterprise']);

  RETURN QUERY SELECT results_eq(
    format('EXECUTE project_scoped_roles(%L, %L, %L, %L)', 'BigCo', 'BigCo User', 'BigCo Engineering', 'skunkworks'),
    'proj_permissions',
    'BigCo User also has "reviewer" at the project scope'
  );

  -- Pipeline Scope

  PREPARE pipeline_scoped_roles(text, text, text, text, text) AS
    SELECT role, scopes FROM scoped_roles($1, $2, $3, $4, $5)
    ORDER BY role;

  RETURN QUERY SELECT results_eq(
    format('EXECUTE pipeline_scoped_roles(%L, %L, %L, %L, %L)', 'BigCo', 'BigCo User', 'BigCo Engineering', 'skunkworks', 'master'),
    'proj_permissions',
    'BigCo User still has its project-scoped  roles at the pipeline scope'
  );

  INSERT INTO pipeline_user_roles(pipeline_id, user_id, role)
  SELECT pipe('BigCo', 'BigCo Engineering', 'skunkworks', 'master'), u('BigCo', 'BigCo User'), role
  FROM unnest(ARRAY['shipper']::delivery_role[]) AS roles(role);

  PREPARE pipe_permissions AS
    VALUES ('admin'::delivery_role, ARRAY['enterprise']),
           ('committer'::delivery_role, ARRAY['enterprise', 'organization']),
           ('reviewer'::delivery_role, ARRAY['enterprise', 'project']),
           ('shipper'::delivery_role, ARRAY['enterprise', 'pipeline']),
           ('observer'::delivery_role, ARRAY['enterprise']);

  RETURN QUERY SELECT results_eq(
    format('EXECUTE pipeline_scoped_roles(%L, %L, %L, %L, %L)', 'BigCo', 'BigCo User', 'BigCo Engineering', 'skunkworks', 'master'),
    'pipe_permissions',
    'BigCo User also has "shipper" at the pipeline scope'
  );

  -- Error conditions
  RETURN QUERY SELECT throws_ok(
    format('SELECT role, scopes FROM scoped_roles(%L, %L, %L, %L, %L)', 'Doge', 'BigCo User', NULL, NULL, NULL),
    'CD003',
    'Enterprise not found',
    '"Doge" is not a real enterprise!'
  );

  RETURN QUERY SELECT throws_ok(
    format('SELECT role, scopes FROM scoped_roles(%L, %L, %L, %L, %L)', 'BigCo', 'Doge', NULL, NULL, NULL),
    'CD004',
    'User not found',
    '"Doge" is not a real user!'
  );

  RETURN QUERY SELECT throws_ok(
    format('SELECT role, scopes FROM scoped_roles(%L, %L, %L, %L, %L)', 'BigCo', 'BigCo User', 'Doge', NULL, NULL),
    'CD005',
    'Organization not found',
    '"Doge" is not a real organization!'
  );

  RETURN QUERY SELECT throws_ok(
    format('SELECT role, scopes FROM scoped_roles(%L, %L, %L, %L, %L)', 'BigCo', 'BigCo User', 'BigCo Engineering', 'Doge', NULL),
    'CD006',
    'Project not found',
    '"Doge" is not a real project!'
  );

  RETURN QUERY SELECT throws_ok(
    format('SELECT role, scopes FROM scoped_roles(%L, %L, %L, %L, %L)', 'BigCo', 'BigCo User', 'BigCo Engineering', 'skunkworks', 'doge'),
    'CD007',
    'Pipeline not found',
    '"doge" is not a real pipeline!'
  );

END;
$$;
