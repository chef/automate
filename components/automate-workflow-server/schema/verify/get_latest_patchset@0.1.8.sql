-- Verify get_latest_patchset

BEGIN;

SELECT has_function_privilege(
  'get_latest_patchset(uuid)',
  'execute');

ROLLBACK;
