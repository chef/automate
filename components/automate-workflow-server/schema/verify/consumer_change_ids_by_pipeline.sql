-- Verify delivery:consumer_change_ids_by_pipeline on pg

BEGIN;

SELECT has_function_privilege(
  'consumer_change_ids_by_pipeline(bigint)',
  'execute');

ROLLBACK;
