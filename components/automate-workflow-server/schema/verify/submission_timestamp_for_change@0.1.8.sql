-- Verify submission_timestamp_for_change

BEGIN;

SELECT has_function_privilege(
  'submission_timestamp_for_change(uuid)',
  'execute');

ROLLBACK;
