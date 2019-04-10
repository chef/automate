-- Deploy submission_timestamp_for_change

BEGIN;

CREATE OR REPLACE FUNCTION submission_timestamp_for_change(p_change_id changes.id%TYPE)
RETURNS cd_timestamp
LANGUAGE SQL STABLE
AS $$
  SELECT submitted_at
  FROM patchsets
  WHERE change_id = p_change_id
    AND sequence_number = 1;
$$;

COMMENT ON FUNCTION submission_timestamp_for_change(p_change_id changes.id%TYPE) IS
$$Retrieves the time when a change was initially submitted.

This is exactly the time that the first patchset for the change is
submitted.$$;

COMMIT;
