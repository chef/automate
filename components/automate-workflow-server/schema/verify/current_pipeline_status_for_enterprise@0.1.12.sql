-- Verify current_pipeline_status_for_enterprise

BEGIN;

SELECT has_function_privilege(
  'current_pipeline_status_for_enterprise(text)',
  'execute');

ROLLBACK;
