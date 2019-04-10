-- Verify patchset_id_from_seq_number

BEGIN;

SELECT has_function_privilege(
  'patchset_id_from_seq_number(smallint, uuid)',
  'execute');

ROLLBACK;
