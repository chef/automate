-- Verify phase_run_summary

BEGIN;

SELECT has_function_privilege(
  'phase_run_summary(uuid)',
  'execute');

ROLLBACK;
