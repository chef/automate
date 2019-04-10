-- Verify delivery:convert_to_local on pg

BEGIN;

SELECT has_function_privilege(
  'convert_to_local(bigint)',
  'execute');

ROLLBACK;
