-- Deploy create_patchset_and_change

-- The idea is that when a user triggers a review from a feature branch, we
-- need to create a change if it doesn't exist, and in any case we need to create
-- a new patchset.

-- This function takes:
--  * an enterprise name
--  * a user name (the submitter)
--  * an organization name
--  * a project name
--  * a pipeline name
--  * a feature branch name
-- and then does exactly what's described above (atomically), returning a
-- `change' singleton if all goes well
-- Returns the created patchset - you can then retrieve the change if needed
-- from the patchset record's change_id field

BEGIN;

CREATE OR REPLACE FUNCTION create_patchset_and_change(
  p_enterprise_name enterprises.name%TYPE,
  p_submitter_name users.name%TYPE,
  p_organization_name organizations.name%TYPE,
  p_project_name projects.name%TYPE,
  p_pipeline_name pipelines.name%TYPE,
  p_feature_branch_name changes.feature_branch%TYPE,
  p_sha patchsets.sha%TYPE
)
RETURNS SETOF patchsets
LANGUAGE plpgsql
AS $$
DECLARE
  v_submitter_id users.id%TYPE;
  v_pipeline_id pipelines.id%TYPE;
  v_change changes%ROWTYPE;
  v_patchset_number patchsets.sequence_number%TYPE;
  v_current_latest_sha patchsets.sha%TYPE;
  v_patchset patchsets%ROWTYPE;
  v_patchset_timestamp patchsets.submitted_at%TYPE;
BEGIN
  SELECT user_id, pipeline_id
  FROM to_ids(p_enterprise_name, p_submitter_name, p_organization_name, p_project_name, p_pipeline_name)
  INTO v_submitter_id, v_pipeline_id;

	v_patchset_timestamp = now();

  SELECT changes.*
  FROM changes
  WHERE pipeline_id = v_pipeline_id
  AND feature_branch = p_feature_branch_name
  AND merge_sha IS NULL
  INTO v_change;

  IF FOUND THEN
    -- we need to know what patchset number this is
    SELECT sequence_number + 1, sha
    FROM patchsets
    WHERE change_id = v_change.id
    ORDER BY id DESC
    INTO v_patchset_number, v_current_latest_sha;
    -- let's check that the SHA differs from the current latest one,
    -- otherwise we reject it (a new patchset can't be identical to
    -- the current latest one)
    IF p_sha = v_current_latest_sha THEN
      RAISE EXCEPTION
        USING ERRCODE = 'CD008',
              MESSAGE = 'New patch set identical to current latest one',
              DETAIL  = 'Sha "' || v_current_latest_sha || '" is the same as '
                        'in latest patch set ' || (v_patchset_number - 1 ),
              HINT    = 'A new patchset has to point to a new SHA';
    END IF;
    -- Update the change for the new patch number and status
    UPDATE changes
    SET latest_patchset_status = 'open',
        latest_patchset = v_patchset_number
    WHERE changes.id = v_change.id;
  ELSE
    -- and then the patchset number is 1, and we don't need to check
    -- any previous SHA, none exists!
    v_patchset_number = 1;
    -- we need to create a new change
    INSERT INTO changes(id, pipeline_id, feature_branch, latest_patchset_status, latest_patchset, submitted_by, submitted_at, pipeline_name_at_creation)
    VALUES (uuid_generate_v4(), v_pipeline_id, p_feature_branch_name, 'open', v_patchset_number, p_submitter_name, v_patchset_timestamp, p_pipeline_name)
    RETURNING changes.*
    INTO v_change;

  END IF;

  -- mark as superseded all other patch sets for this change that were open
  UPDATE patchsets
  SET status = 'superseded'
  WHERE change_id = v_change.id
  AND status = 'open';

  -- finally we create the patchset
  INSERT INTO patchsets(change_id, sequence_number, submitter_id, submitted_at, status, sha)
  VALUES (v_change.id, v_patchset_number, v_submitter_id, v_patchset_timestamp, 'open', p_sha)
  RETURNING patchsets.*
  INTO v_patchset;

  RETURN NEXT v_patchset;
END;
$$;

COMMIT;
