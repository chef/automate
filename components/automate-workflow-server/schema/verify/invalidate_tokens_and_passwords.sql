-- Verify invalidate_tokens_and_passwords

BEGIN;

SELECT has_function_privilege(
  'invalidate(credential)',
  'execute');
SELECT has_function_privilege(
  'invalidate(credential, TEXT)',
  'execute');
SELECT has_function_privilege(
  'invalidate(credential, TEXT, TEXT)',
  'execute');

SELECT has_function_privilege(
  'credential_table(credential)',
  'execute');

ROLLBACK;
