-- Verify raise_user_type_change_exception

BEGIN;

SELECT has_function_privilege(
  'raise_user_type_change_exception()',
  'execute');

ROLLBACK;
