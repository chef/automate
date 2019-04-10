-- Verify get_change

BEGIN;

SELECT has_function_privilege(
  'get_change(uuid)',
  'execute');

ROLLBACK;
