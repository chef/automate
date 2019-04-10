CREATE OR REPLACE FUNCTION test_add_to_changeset_function()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
DECLARE
  v_pipeline_id pipelines.id%TYPE := pipe('BigCo','BigCo Engineering','skunkworks','master');
  v_change changes%ROWTYPE;
  v_changeset changesets%ROWTYPE;
BEGIN

  -- with no changeset open for a pipeline
  RETURN QUERY SELECT is_empty(
    format('SELECT * FROM changesets WHERE status = %L AND pipeline_id = %L',
    'open', v_pipeline_id),
    'Testing pipeline should not have an open changeset'
  );

  -- add a change to a changeset
  INSERT INTO changes(id,
                      pipeline_id,
                      feature_branch,
                      merge_sha,
                      title,
                      description,
                      approved_by,
                      latest_patchset_status,
                      latest_patchset,
                      submitted_at,
                      submitted_by,
                      pipeline_name_at_creation)
  VALUES(uuid_generate_v4(),
         v_pipeline_id,
         'pgtap/feature_branch',
         'deadbeef',
         'The title of pgtap/feature_branch',
         'The description of pgtap/feature_branch',
         'pgtap_approver',
         'merged',
         1,
         '2015-08-31 06:35:51.196365-07',
         'pgtap_submitter',
         'master')
  RETURNING * INTO v_change;

  SELECT * FROM add_to_changeset(v_change.id) LIMIT 1
  INTO v_changeset;
  -- RAISE WARNING 'New changeset = %', v_changeset;

  -- assert that the returned changeset is open and the ID matches
  --    the one on the change now.
  RETURN QUERY SELECT is(
    v_changeset.status,
    'open',
    'New Changeset should be open');

  RETURN QUERY SELECT is(
    changeset_id,
    v_changeset.id,
    'Change should be added to changeset')
  FROM changes
  WHERE id = v_change.id;

  -- with a changeset open for a pipeline
  -- add a change to the changeset
  -- assert it remains open, the change is linked, and the existing
  --   change remains

RETURN;
END;
$$;
