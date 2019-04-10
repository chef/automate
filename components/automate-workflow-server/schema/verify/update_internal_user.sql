-- Verify update_internal_user

BEGIN;

SELECT has_function_privilege(
  'update_internal_user()',
  'execute');

ROLLBACK;
