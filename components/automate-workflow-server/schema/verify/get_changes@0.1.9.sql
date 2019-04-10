-- Verify get_changes

BEGIN;

SELECT has_function_privilege(
  'get_changes(text, text, text, text, cd_patchset_status, uuid, boolean, integer)',
  'execute');

ROLLBACK;
