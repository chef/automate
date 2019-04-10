-- Deploy current_pipeline_snapshot_for_enterprise

BEGIN;

DROP FUNCTION current_pipeline_snapshot_for_enterprise(p_enterprise_name enterprises.name%TYPE);

COMMIT;
