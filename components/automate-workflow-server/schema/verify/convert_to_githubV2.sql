-- Verify delivery:convert_to_githubV2 on pg

BEGIN;

SELECT has_function_privilege(
  'convert_to_githubV2(bigint, text, text)',
  'execute');

ROLLBACK;
