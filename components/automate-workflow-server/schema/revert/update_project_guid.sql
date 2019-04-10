-- Revert update_project_guid

BEGIN;

DROP FUNCTION IF EXISTS update_project_guid();

COMMIT;
