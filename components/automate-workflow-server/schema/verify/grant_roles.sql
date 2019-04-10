-- Verify grant_roles

BEGIN;

SELECT has_function_privilege(
  'grant_roles(text, text, text, text, text, text[])',
  'execute');

ROLLBACK;
