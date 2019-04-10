-- Verify phases_for_stage

BEGIN;

SELECT has_function_privilege(
  'phases_for_stage(text)',
  'execute');

ROLLBACK;
