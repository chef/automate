CREATE OR REPLACE FUNCTION test_revoke_roles_function()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
DECLARE
  test_enterprise CONSTANT enterprises.name%TYPE = 'BigCo';
  test_user CONSTANT users.name%TYPE  = 'BigCo User';
  test_organization CONSTANT organizations.name%TYPE = 'BigCo Engineering';
  test_project CONSTANT projects.name%TYPE = 'skunkworks';
  test_pipeline CONSTANT pipelines.name%TYPE = 'master';

  test_enterprise_id CONSTANT enterprises.id%TYPE = ent(test_enterprise);
  test_user_id CONSTANT users.id%TYPE = u(test_enterprise, test_user);
  test_organization_id CONSTANT organizations.id%TYPE = org(test_enterprise, test_organization);
  test_project_id CONSTANT projects.id%TYPE = proj(test_enterprise, test_organization, test_project);
  test_pipeline_id CONSTANT pipelines.id%TYPE = pipe(test_enterprise, test_organization, test_project, test_pipeline);

BEGIN
  -- Set all the roles at all the scopes to start with
  PERFORM grant_roles(test_enterprise, test_user, NULL, NULL, NULL, ARRAY['admin', 'committer', 'reviewer', 'shipper', 'observer']);
  PERFORM grant_roles(test_enterprise, test_user, test_organization, NULL, NULL, ARRAY['admin', 'committer', 'reviewer', 'shipper', 'observer']);
  PERFORM grant_roles(test_enterprise, test_user, test_organization, test_project, NULL, ARRAY['admin', 'committer', 'reviewer', 'shipper', 'observer']);
  PERFORM grant_roles(test_enterprise, test_user, test_organization, test_project, test_pipeline, ARRAY['admin', 'committer', 'reviewer', 'shipper', 'observer']);

  -- Last sanity check, to make sure we had all the right variables
  -- set above. If all that worked as expected, then we should see
  -- that we have those three roles granted at every scope when we're
  -- down at the pipeline level
  PREPARE all_permissions AS
    VALUES ('admin'::delivery_role, ARRAY['enterprise', 'organization', 'project', 'pipeline']),
           ('committer'::delivery_role, ARRAY['enterprise', 'organization', 'project', 'pipeline']),
           ('reviewer'::delivery_role, ARRAY['enterprise', 'organization', 'project', 'pipeline']),
           ('shipper'::delivery_role, ARRAY['enterprise', 'organization', 'project', 'pipeline']),
           ('observer'::delivery_role, ARRAY['enterprise', 'organization', 'project', 'pipeline']);

  RETURN QUERY SELECT results_eq(
    format('SELECT * FROM scoped_roles(%L, %L, %L, %L, %L)', test_enterprise, test_user, test_organization, test_project, test_pipeline),
    'all_permissions',
    'We have all the roles we expect'
  );

  -- Enterprise roles revokes!

  PREPARE enterprise_roles(bigint, bigint) AS
  SELECT role FROM enterprise_user_roles
  WHERE enterprise_id = $1
    AND user_id = $2
  ORDER BY role;

  RETURN QUERY SELECT lives_ok(
    format('SELECT revoke_roles(%L, %L, NULL, NULL, NULL, %L)', test_enterprise, test_user, ARRAY['admin']),
    'Revoke "admin" at the enterprise scope'
  );

  RETURN QUERY SELECT results_eq(
    format('EXECUTE enterprise_roles(%L, %L)', test_enterprise_id, test_user_id),
    ARRAY['committer', 'reviewer', 'shipper', 'observer']::delivery_role[],
    'The user still has the rest of the roles at the enterprise scope now!'
  );

  RETURN QUERY SELECT lives_ok(
    format('SELECT revoke_roles(%L, %L, NULL, NULL, NULL, %L)', test_enterprise, test_user, ARRAY['admin']),
    'Should be able to revoke "admin" again with no ill effects'
  );

  RETURN QUERY SELECT results_eq(
    format('EXECUTE enterprise_roles(%L, %L)', test_enterprise_id, test_user_id),
    ARRAY['committer', 'reviewer', 'shipper', 'observer']::delivery_role[],
    'The user still has the non-admin roles roles at the enterprise scope'
  );

  RETURN QUERY SELECT lives_ok(
    format('SELECT revoke_roles(%L, %L, NULL, NULL, NULL, %L)', test_enterprise, test_user, ARRAY['admin', 'observer', 'reviewer']),
    'Should be able to revoke several roles, some of which overlap with previously revoked ones, with no ill effects'
  );

  RETURN QUERY SELECT results_eq(
    format('EXECUTE enterprise_roles(%L, %L)', test_enterprise_id, test_user_id),
    ARRAY['committer', 'shipper']::delivery_role[],
    'The user has all the roles they should at the enterprise scope'
  );

  RETURN QUERY SELECT results_eq(
    format('SELECT * FROM revoke_roles(%L, %L, NULL, NULL, NULL, %L)', test_enterprise, test_user, ARRAY['admin', 'observer', 'reviewer']),
    format('SELECT * FROM scoped_roles(%L, %L, NULL, NULL, NULL)', test_enterprise, test_user),
    'Returns scoped role information'
  );

  -- Organization roles revokes!

  PREPARE organization_roles(bigint, bigint) AS
  SELECT role FROM organization_user_roles
  WHERE organization_id = $1
    AND user_id = $2
  ORDER BY role;

  RETURN QUERY SELECT lives_ok(
    format('SELECT revoke_roles(%L, %L, %L, NULL, NULL, %L)', test_enterprise, test_user, test_organization, ARRAY['admin']),
    'Revoke "admin" at the organization scope'
  );

  RETURN QUERY SELECT results_eq(
    format('EXECUTE organization_roles(%L, %L)', test_organization_id, test_user_id),
    ARRAY['committer', 'reviewer', 'shipper', 'observer']::delivery_role[],
    'The user still has the rest of the roles at the organization scope'
  );

  RETURN QUERY SELECT lives_ok(
    format('SELECT revoke_roles(%L, %L, %L, NULL, NULL, %L)', test_enterprise, test_user, test_organization, ARRAY['admin', 'observer', 'reviewer']),
    'Should be able to revoke several roles, some of which overlap with previously revoked ones, with no ill effects'
  );

  RETURN QUERY SELECT results_eq(
    format('EXECUTE organization_roles(%L, %L)', test_organization_id, test_user_id),
    ARRAY['committer', 'shipper']::delivery_role[],
    'The user has all the roles they should at the organization scope'
  );

 RETURN QUERY SELECT results_eq(
    format('SELECT * FROM revoke_roles(%L, %L, %L, NULL, NULL, %L)', test_enterprise, test_user, test_organization, ARRAY['admin', 'observer', 'reviewer']),
    format('SELECT * FROM scoped_roles(%L, %L, %L, NULL, NULL)', test_enterprise, test_user, test_organization),
    'Returns scoped role information'
  );

  -- Project Revokes!

  PREPARE project_roles(bigint, bigint) AS
  SELECT role FROM project_user_roles
  WHERE project_id = $1
    AND user_id = $2
  ORDER BY role;

   RETURN QUERY SELECT lives_ok(
    format('SELECT revoke_roles(%L, %L, %L, %L, NULL, %L)', test_enterprise, test_user, test_organization, test_project, ARRAY['admin']),
    'Revoke "admin" at the project scope'
  );

  RETURN QUERY SELECT results_eq(
    format('EXECUTE project_roles(%L, %L)', test_project_id, test_user_id),
    ARRAY['committer', 'reviewer', 'shipper', 'observer']::delivery_role[],
    'The user still has the rest of the roles at the project scope'
  );

  RETURN QUERY SELECT lives_ok(
    format('SELECT revoke_roles(%L, %L, %L, %L, NULL, %L)', test_enterprise, test_user, test_organization, test_project, ARRAY['admin', 'observer', 'reviewer']),
    'Should be able to revoke several roles, some of which overlap with previously revoked ones, with no ill effects'
  );

  RETURN QUERY SELECT results_eq(
    format('EXECUTE project_roles(%L, %L)', test_project_id, test_user_id),
    ARRAY['committer', 'shipper']::delivery_role[],
    'The user has all the roles they should at the project scope'
  );

  RETURN QUERY SELECT results_eq(
    format('SELECT * FROM revoke_roles(%L, %L, %L, %L, NULL, %L)', test_enterprise, test_user, test_organization, test_project, ARRAY['admin', 'observer', 'reviewer']),
    format('SELECT * FROM scoped_roles(%L, %L, %L, %L, NULL)', test_enterprise, test_user, test_organization, test_project),
    'Returns scoped role information'
  );


  -- Pipelines! FOR GREAT JUSTICE!

  PREPARE pipeline_roles(bigint, bigint) AS
  SELECT role FROM pipeline_user_roles
  WHERE pipeline_id = $1
    AND user_id = $2
  ORDER BY role;

  RETURN QUERY SELECT lives_ok(
    format('SELECT revoke_roles(%L, %L, %L, %L, %L, %L)', test_enterprise, test_user, test_organization, test_project, test_pipeline, ARRAY['admin']),
    'Revoke "admin" at the pipeline scope'
  );

  RETURN QUERY SELECT results_eq(
    format('EXECUTE pipeline_roles(%L, %L)', test_pipeline_id, test_user_id),
    ARRAY['committer', 'reviewer', 'shipper', 'observer']::delivery_role[],
    'The user still has the rest of the roles at the pipeline scope'
  );

  RETURN QUERY SELECT lives_ok(
    format('SELECT revoke_roles(%L, %L, %L, %L, %L, %L)', test_enterprise, test_user, test_organization, test_project, test_pipeline, ARRAY['admin', 'observer', 'reviewer']),
    'Should be able to revoke several roles, some of which overlap with previously revoked ones, with no ill effects'
  );

  RETURN QUERY SELECT results_eq(
    format('EXECUTE pipeline_roles(%L, %L)', test_pipeline_id, test_user_id),
    ARRAY['committer', 'shipper']::delivery_role[],
    'The user has all the roles they should at the pipeline scope'
  );

  RETURN QUERY SELECT results_eq(
    format('SELECT * FROM revoke_roles(%L, %L, %L, %L, %L, %L)', test_enterprise, test_user, test_organization, test_project, test_pipeline, ARRAY['admin', 'observer', 'reviewer']),
    format('SELECT * FROM scoped_roles(%L, %L, %L, %L, %L)', test_enterprise, test_user, test_organization, test_project, test_pipeline),
    'Returns scoped role information'
  );

  -- Last sanity check, to make sure we had all the right variables
  -- set above. If all that worked as expected, then we should see
  -- that we have those two roles granted at every scope when we're
  -- down at the pipeline level
  PREPARE remaining_permissions AS
    VALUES ('committer'::delivery_role, ARRAY['enterprise', 'organization', 'project', 'pipeline']),
           ('shipper'::delivery_role, ARRAY['enterprise', 'organization', 'project', 'pipeline']);

  RETURN QUERY SELECT results_eq(
    format('SELECT * FROM scoped_roles(%L, %L, %L, %L, %L)', test_enterprise, test_user, test_organization, test_project, test_pipeline),
    'remaining_permissions',
    'We have all the roles we expect'
  );

END;
$$;
