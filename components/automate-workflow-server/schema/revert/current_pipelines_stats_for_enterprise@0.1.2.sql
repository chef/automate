-- Revert current_pipelines_stats_for_enterprise

BEGIN;

DROP FUNCTION IF EXISTS current_pipelines_stats_for_enterprise(p_enterprise_name enterprises.name%TYPE);

COMMIT;
