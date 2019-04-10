-- Deploy merge_change
-- requires: changes
-- requires: changesets

-- This function records a merge SHA for the given change.
-- Returns the updated change record.

BEGIN;

DROP FUNCTION IF EXISTS merge_change(
  p_change_id changes.id%TYPE,
  p_merge_sha changes.merge_sha%TYPE
);

CREATE OR REPLACE FUNCTION merge_change(
  p_change_id changes.id%TYPE,
  p_merge_sha changes.merge_sha%TYPE,
  p_approver changes.approved_by%TYPE
)
RETURNS SETOF changes ROWS 1
LANGUAGE plpgsql
AS $$
DECLARE
  v_latest_patchset patchsets.sequence_number%TYPE;
BEGIN
  SELECT latest_patchset
  FROM changes
  WHERE id = p_change_id
  INTO v_latest_patchset;

  IF NOT FOUND THEN
    RAISE EXCEPTION
      USING ERRCODE = 'CD018',
            MESSAGE = 'Unknown change',
            DETAIL  = 'Change "' || p_change_id || '" does not exist',
            HINT    = 'Cannot merge thin air';
  END IF;

  -- update the latest patchset, set status as merged
  UPDATE patchsets
     SET status = 'merged'
   WHERE change_id = p_change_id
     AND sequence_number = v_latest_patchset;

  RETURN QUERY
  UPDATE changes
     SET merge_sha = p_merge_sha,
         latest_patchset_status = 'merged',
         approved_at = NOW(),
         approved_by = p_approver
   WHERE id = p_change_id
  RETURNING changes.*;
END;
$$;

COMMIT;
