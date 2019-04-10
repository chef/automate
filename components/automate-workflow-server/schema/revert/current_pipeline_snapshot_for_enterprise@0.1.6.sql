-- Revert current_pipeline_snapshot_for_enterprise

BEGIN;

DROP FUNCTION IF EXISTS current_pipeline_snapshot_for_enterprise(enterprises.name%TYPE);

COMMIT;
