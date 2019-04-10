-- Verify current_pipelines_stats_for_enterprise

BEGIN;

SELECT has_function_privilege(
  'current_pipelines_stats_for_enterprise(text)',
  'execute');

ROLLBACK;
