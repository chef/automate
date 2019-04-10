-- Verify insert_internal_user

BEGIN;

SELECT has_function_privilege(
  'insert_internal_user()',
  'execute');

ROLLBACK;
