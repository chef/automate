-- Revert submission_timestamp_for_change

BEGIN;

DROP FUNCTION IF EXISTS submission_timestamp_for_change(p_change_id changes.id%TYPE);

COMMIT;
