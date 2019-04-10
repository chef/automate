-- Verify assert_patchset_in_change

BEGIN;

SELECT has_function_privilege(
  'assert_patchset_in_change(bigint, uuid)',
  'execute');

ROLLBACK;
