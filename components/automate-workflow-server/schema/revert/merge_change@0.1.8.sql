-- Deploy merge_change
-- requires: changes
-- requires: changesets

-- This function records a merge SHA for the given change,
-- and links it to the open changeset for the change's
-- pipeline (creating it if necessary)
-- That's why we need this to be a DB fun (no transaction
-- in pgsql)
-- Returns the updated change record

BEGIN;

CREATE OR REPLACE FUNCTION merge_change(
  p_change_id changes.id%TYPE,
  p_merge_sha changes.merge_sha%TYPE
)
RETURNS SETOF changes ROWS 1
LANGUAGE plpgsql
AS $$
DECLARE
  v_pipeline_id pipelines.id%TYPE;
  v_changeset_id changesets.id%TYPE;
BEGIN
  SELECT pipeline_id
  FROM changes
  WHERE id = p_change_id
  INTO v_pipeline_id;

  IF NOT FOUND THEN
    RAISE EXCEPTION
      USING ERRCODE = 'CD018',
            MESSAGE = 'Unknown change',
            DETAIL  = 'Change "' || p_change_id || '" does not exist',
            HINT    = 'Cannot merge thin air';
  END IF;

  SELECT id
  FROM changesets
  WHERE pipeline_id = v_pipeline_id
  AND status = 'open'
  INTO v_changeset_id;

  -- if there's no open changeset for that pipeline,
  -- let's create it
  IF NOT FOUND THEN
    INSERT INTO changesets(id, pipeline_id, status)
    VALUES (uuid_generate_v4(), v_pipeline_id, 'open')
    RETURNING id
    INTO v_changeset_id;
  END IF;

  RETURN QUERY
  UPDATE changes
  SET merge_sha = p_merge_sha,
      latest_patchset_status = 'merged',
      changeset_id = v_changeset_id
  WHERE id = p_change_id
  RETURNING changes.*;
END;
$$;

COMMIT;
