-- Verify delivery:convert_to_bb on pg

BEGIN;

SELECT has_function_privilege(
  'convert_to_bb(bigint, text, text)',
  'execute');

ROLLBACK;
