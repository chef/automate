CREATE OR REPLACE FUNCTION test_convert_to_githubV2()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
DECLARE
  test_enterprise_name CONSTANT enterprises.name%TYPE = 'BigCo';
  test_organization_name CONSTANT organizations.name%TYPE = 'BigCo Engineering';
  test_project_name CONSTANT projects.name%TYPE = 'convert_local_to_githubV2';
  test_scm_module CONSTANT projects.scm_module%TYPE = 'github_scm';
  test_repo_owner CONSTANT project_github_metadata.repo_owner%TYPE = 'tom';
  test_repo_name CONSTANT project_github_metadata.repo_name%TYPE = 'delivery';
  test_organization_id projects.organization_id%TYPE;
  test_project_id projects.id%TYPE;
  test_project projects%ROWTYPE;
BEGIN

  SELECT organization_id
  FROM to_ids(test_enterprise_name, NULL, test_organization_name, NULL, NULL)
  INTO test_organization_id;

  INSERT INTO projects(organization_id, name) VALUES (test_organization_id, test_project_name);

  SELECT project_id
  FROM to_ids(test_enterprise_name, NULL, test_organization_name, test_project_name, NULL)
  INTO test_project_id;

  SELECT *
  FROM convert_to_githubV2(test_project_id, test_repo_owner, test_repo_name)
  INTO test_project;

  RETURN QUERY SELECT results_eq(
    format('SELECT count(1) FROM projects WHERE id = %L AND scm_module = %L', test_project_id, test_scm_module),
    ARRAY[1]::BIGINT[],
    'SCM module was updated to Github'
  );

  RETURN QUERY SELECT results_eq(
    format('SELECT count(1) FROM project_github_metadata WHERE project_id = %L AND repo_owner = %L AND repo_name = %L AND token = %L', test_project_id, test_repo_owner, test_repo_name, 'unused'),
    ARRAY[1]::BIGINT[],
    'Github metadata got created'
  );
END;
$$;
