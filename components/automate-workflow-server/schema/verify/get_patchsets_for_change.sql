-- Verify get_patchsets_for_change

BEGIN;

SELECT has_function_privilege(
  'get_patchsets_for_change(uuid)',
  'execute');

ROLLBACK;
