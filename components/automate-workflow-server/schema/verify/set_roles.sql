-- Verify set_roles

BEGIN;

SELECT has_function_privilege(
  'set_roles(text, text, text, text, text, text[])',
  'execute');

ROLLBACK;
