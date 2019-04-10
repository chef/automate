-- Verify stage_run_summary

BEGIN;

SELECT has_function_privilege(
  'stage_run_summary(uuid)',
  'execute');

ROLLBACK;
