CREATE OR REPLACE FUNCTION test_convert_to_local()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
DECLARE
  test_enterprise_name CONSTANT enterprises.name%TYPE = 'BigCo';
  test_organization_name CONSTANT organizations.name%TYPE = 'BigCo Engineering';
  test_project_name CONSTANT projects.name%TYPE = 'awesomeworks';
  test_pipeline_name CONSTANT pipelines.name%TYPE = 'master';
  test_scm_module CONSTANT projects.scm_module%TYPE = 'bitbucket_scm';
  test_bitbucket_project CONSTANT project_bitbucket_metadata.bitbucket_project%TYPE = 'tom';
  test_repo_name CONSTANT project_bitbucket_metadata.repo_name%TYPE = 'delivery';
  test_organization_id projects.organization_id%TYPE;
  test_project_id projects.id%TYPE;
  test_project projects%ROWTYPE;
  test_pipeline_id pipelines.id%TYPE;
  test_change changes%ROWTYPE;
BEGIN

  -- Create bitbucket project and project metadata
  SELECT *
  FROM create_bitbucket_scm_project(test_enterprise_name, test_organization_name, test_project_name, test_pipeline_name, test_scm_module, test_bitbucket_project, test_repo_name)
  INTO test_project;

  -- Create changes for project and change metadata
  SELECT pipeline_id
  FROM to_ids(test_enterprise_name, NULL, test_organization_name, test_project_name, test_pipeline_name)
  INTO test_pipeline_id;

  INSERT INTO changes(id, pipeline_id, feature_branch, latest_patchset_status, latest_patchset, submitted_at, submitted_by, pipeline_name_at_creation)
  VALUES (uuid_generate_v4(), test_pipeline_id, test_pipeline_name, 'open', 1, now(), 'jon', test_pipeline_name)
  RETURNING * INTO test_change;

  INSERT INTO scm_changes(change_id, pr_id, pr_url)
  VALUES (test_change.id, 1, 'http://bitbucket.pr');

  -- Perform the function
  SELECT * 
  FROM convert_to_local(test_project.id)
  INTO test_project;

  -- Assert
  RETURN QUERY SELECT results_eq(
    format('SELECT count(1) FROM projects WHERE id = %L AND scm_module = %L', test_project.id, 'deliv_scm_local'),
    ARRAY[1]::BIGINT[],
    'Project SCM module got updated to "deliv_scm_local"'
  );

  RETURN QUERY SELECT results_eq(
    format('SELECT count(1) FROM project_bitbucket_metadata WHERE project_id = %L', test_project.id),
    ARRAY[0]::BIGINT[],
    'Project bitbucket metadata got deleted'
  );

  RETURN QUERY SELECT results_eq(
    format('SELECT count(1) FROM scm_changes WHERE change_id = %L', test_change.id),
    ARRAY[0]::BIGINT[],
    'Change bitbucket metadata got deleted'
  );

END;
$$;
