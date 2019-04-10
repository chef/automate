CREATE OR REPLACE FUNCTION test_create_patchset_and_change()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
DECLARE
  test_enterprise CONSTANT enterprises.name%TYPE = 'BigCo';
  test_user CONSTANT users.name%TYPE = 'BigCo Chaos Monkey';
  test_organization CONSTANT organizations.name%TYPE = 'BigCo Engineering';
  test_project CONSTANT projects.name%TYPE = 'skunkworks';
  test_pipeline CONSTANT pipelines.name%TYPE = 'master';
  test_feature_branch CONSTANT changes.feature_branch%TYPE = 'bc_eng/feat_branch';

  test_pipeline_id CONSTANT pipelines.id%TYPE = pipe(test_enterprise, test_organization, test_project, test_pipeline);

  test_patchset_1 patchsets;
  test_patchset_2 patchsets;
  test_patchset_3 patchsets;
BEGIN

  -- we create a new patchset, and that should also create a new change
  SELECT *
  FROM create_patchset_and_change(test_enterprise, test_user, test_organization, test_project, test_pipeline, test_feature_branch, 'abcdef')
  INTO test_patchset_1;

  -- The change should have been created
  RETURN QUERY SELECT results_eq(
    format('SELECT count(1) FROM changes WHERE pipeline_id = %L AND feature_branch = %L AND merge_sha IS NULL AND id = %L',
      test_pipeline_id, test_feature_branch, test_patchset_1.change_id),
    ARRAY[1]::BIGINT[],
    'Change got created'
  );

  -- the sequence_number should be 1
  -- and the status 'open'
  RETURN QUERY SELECT is(
    test_patchset_1.sequence_number,
    1::SMALLINT,
    'First patchset''s sequence_number is 1'
  );
  RETURN QUERY SELECT is(
    test_patchset_1.status,
    'open',
    'First patchset''s review is open'
  );

  -- Then we update it
  SELECT *
  FROM create_patchset_and_change(test_enterprise, test_user, test_organization, test_project, test_pipeline, test_feature_branch, 'abc123')
  INTO test_patchset_2;

  RETURN QUERY SELECT results_eq(
    format('SELECT count(1) FROM changes WHERE pipeline_id = %L AND feature_branch = %L AND merge_sha IS NULL',
      test_pipeline_id, test_feature_branch),
    ARRAY[1]::BIGINT[],
    'No new change was created'
  );

  -- that should have created a second patchset
  RETURN QUERY SELECT results_eq(
    format('SELECT sequence_number FROM patchsets WHERE change_id = %L ORDER BY id',
      test_patchset_2.change_id),
    ARRAY[1, 2]::SMALLINT[],
    'Second patchset got created'
  );
  -- first's patch set review status should now be superseded
  RETURN QUERY SELECT results_eq(
    format('SELECT status FROM patchsets WHERE change_id = %L ORDER BY id',
      test_patchset_2.change_id),
    ARRAY['superseded', 'open']::cd_patchset_status[],
    'Second patchset is now the only one to be reviewed'
  );

  -- And for good measure, let's check that once this change gets merged, we can
  -- re-create a new change with the same feature branch name!
  UPDATE changes
  SET merge_sha = 'fake_sha'
  WHERE id = test_patchset_2.change_id;

  SELECT *
  FROM create_patchset_and_change(test_enterprise, test_user, test_organization, test_project, test_pipeline, test_feature_branch, 'abcdef')
  INTO test_patchset_3;

  RETURN QUERY SELECT isnt(
    test_patchset_1.change_id,
    test_patchset_3.change_id,
    'A new change got created'
  );

  -- Last but not least, let's check we can't create a patchset with the same
  -- SHA as the current latest patch set
  RETURN QUERY SELECT throws_ok(
    format('SELECT create_patchset_and_change(%L, %L, %L, %L, %L, %L, %L)',
      test_enterprise, test_user, test_organization, test_project,
      test_pipeline, test_feature_branch, 'abcdef'),
    'CD008',
    'New patch set identical to current latest one'
  );

END;
$$;
