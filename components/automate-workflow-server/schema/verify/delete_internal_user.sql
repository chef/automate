-- Verify delete_internal_user

BEGIN;

SELECT has_function_privilege(
  'delete_internal_user()',
  'execute');

ROLLBACK;
