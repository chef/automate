-- Verify assign_token

BEGIN;

SELECT has_function_privilege(
  'assign_token(TEXT, TEXT, TEXT)',
  'execute');

ROLLBACK;
