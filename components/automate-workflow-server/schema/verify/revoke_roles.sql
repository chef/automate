-- Verify revoke_roles

BEGIN;

SELECT has_function_privilege(
  'revoke_roles(text, text, text, text, text, text[])',
  'execute');

ROLLBACK;
