-- Verify create_patchset_and_change

BEGIN;

SELECT has_function_privilege(
  'create_patchset_and_change(text, text, text, text, text, text, text)',
  'execute');

ROLLBACK;
