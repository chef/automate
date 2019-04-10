-- Verify effective_roles_at_enterprise

BEGIN;

SELECT has_function_privilege(
  'effective_roles_at_enterprise(TEXT, TEXT)',
  'execute');

ROLLBACK;
