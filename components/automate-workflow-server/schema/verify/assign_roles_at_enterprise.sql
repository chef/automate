-- Verify assign_roles_at_enterprise

BEGIN;

SELECT has_function_privilege(
  'assign_roles_at_enterprise(text, text, text[])',
  'execute');

ROLLBACK;
