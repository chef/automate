-- Verify all_stages

BEGIN;

SELECT has_function_privilege(
  'all_stages()',
  'execute');

ROLLBACK;
