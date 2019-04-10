-- Revert all_stages

BEGIN;

DROP FUNCTION IF EXISTS all_stages();

COMMIT;
