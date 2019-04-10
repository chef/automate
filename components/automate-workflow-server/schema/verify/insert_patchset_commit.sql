-- Verify insert_patchset_commit

BEGIN;

SELECT has_function_privilege(
  'insert_patchset_commit(bigint, bigint, text, text, text)',
  'execute');

ROLLBACK;
