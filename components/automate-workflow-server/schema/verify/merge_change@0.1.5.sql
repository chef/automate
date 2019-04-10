-- Verify merge_change

BEGIN;

SELECT has_function_privilege(
  'merge_change(uuid, text)',
  'execute');

ROLLBACK;
