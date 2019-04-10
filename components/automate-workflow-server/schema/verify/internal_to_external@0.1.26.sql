-- Verify internal_to_external

BEGIN;

SELECT has_function_privilege(
  'utility.internal_to_external(text, text)',
  'execute');

ROLLBACK;
