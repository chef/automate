-- Verify to_ids

BEGIN;

SELECT has_function_privilege(
  'to_ids(text, text, text, text, text)',
  'execute');

ROLLBACK;
