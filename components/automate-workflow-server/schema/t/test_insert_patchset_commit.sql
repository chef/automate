CREATE OR REPLACE FUNCTION test_insert_patchset_commit()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
DECLARE
  test_enterprise CONSTANT enterprises.name%TYPE = 'BigCo';
  test_user CONSTANT users.name%TYPE = 'BigCo Chaos Monkey';
  test_organization CONSTANT organizations.name%TYPE = 'BigCo Engineering';
  test_project CONSTANT projects.name%TYPE = 'skunkworks';
  test_pipeline CONSTANT pipelines.name%TYPE = 'master';
  test_feature_branch CONSTANT changes.feature_branch%TYPE = 'bc_eng/test_insert_patchset_commits';
  test_sha CONSTANT patchsets.sha%TYPE = 'abc712';

  test_project_id CONSTANT projects.id%TYPE = proj(test_enterprise, test_organization, test_project);
  test_patchset_id_1 patchsets.id%TYPE;
  test_patchset_id_2 patchsets.id%TYPE;

  test_project_commit project_commits;
BEGIN
  SELECT id
  FROM create_patchset_and_change(test_enterprise, test_user, test_organization, test_project, test_pipeline, test_feature_branch, test_sha)
  INTO test_patchset_id_1;

  -- insert a patchset commit
  RETURN QUERY SELECT lives_ok(
    format('SELECT * FROM insert_patchset_commit(%L, %L, %L, ''commit subject'', ''commit body'')',
           test_project_id, test_patchset_id_1, test_sha),
    'Insert a new project commit'
  );

  -- should have created the project commit
  SELECT *
  FROM project_commits
  WHERE project_id = test_project_id
  AND sha = test_sha
  INTO test_project_commit;
  RETURN QUERY SELECT is(
    test_project_commit.subject,
    'commit subject'
  );
  RETURN QUERY SELECT is(
    test_project_commit.body,
    'commit body'
  );

  -- and also the corresponding entry in the pivot table
  RETURN QUERY SELECT results_eq(
    format('SELECT count(1) from patchset_project_commits WHERE patchset_id = %L AND project_commit_id = %L',
           test_patchset_id_1, test_project_commit.id),
    ARRAY[1]::BIGINT[]
  );

  -- now re-inserting the same commit as part of another patchset
  SELECT id
  FROM create_patchset_and_change(test_enterprise, test_user, test_organization, test_project, test_pipeline, test_feature_branch, '28282')
  INTO test_patchset_id_2;

  RETURN QUERY SELECT lives_ok(
    format('SELECT * FROM insert_patchset_commit(%L, %L, %L, ''commit subject'', ''commit body'')',
           test_project_id, test_patchset_id_2, test_sha),
    'Re-inserting the same commit as part of another patchset'
  );

  -- should have created a new entry in the pivot table
  RETURN QUERY SELECT results_eq(
    format('SELECT count(1) from patchset_project_commits WHERE patchset_id = %L AND project_commit_id = %L',
           test_patchset_id_2, test_project_commit.id),
    ARRAY[1]::BIGINT[]
  );
END;
$$;
