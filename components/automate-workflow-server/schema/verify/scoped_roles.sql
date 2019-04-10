-- Verify scoped_roles

BEGIN;

SELECT has_function_privilege(
  'scoped_roles(text, text, text, text, text)',
  'execute');

ROLLBACK;
