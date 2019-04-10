-- Verify update_project_guid

BEGIN;

SELECT has_function_privilege(
  'update_project_guid()',
  'execute');

ROLLBACK;
