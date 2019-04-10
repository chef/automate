-- Deploy submission_timestamp_for_change

-- The need for this function was removed when the submission timestamp was
-- added to the changes table.

BEGIN;
DROP FUNCTION IF EXISTS submission_timestamp_for_change(p_change_id changes.id%TYPE);

COMMIT;
