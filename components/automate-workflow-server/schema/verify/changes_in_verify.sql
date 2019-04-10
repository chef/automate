-- Verify changes_in_verify

BEGIN;

SELECT has_function_privilege(
  'changes_in_verify(TEXT, TEXT)',
  'execute');

ROLLBACK;
