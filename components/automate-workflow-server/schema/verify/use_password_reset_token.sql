-- Verify delivery:use_password_reset_token on pg

BEGIN;

SELECT has_function_privilege(
  'use_password_reset_token(TEXT, TEXT, TEXT)',
  'execute');

ROLLBACK;
