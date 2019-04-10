-- Verify delivery:assign_password_reset_token on pg

BEGIN;

SELECT has_function_privilege(
  'assign_password_reset_token(TEXT, TEXT, TEXT, INTERVAL)',
  'execute');

ROLLBACK;
