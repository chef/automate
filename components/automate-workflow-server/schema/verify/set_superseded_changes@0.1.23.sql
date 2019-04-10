-- Verify delivery:set_superseded_changes on pg

BEGIN;

SELECT has_function_privilege(
  'set_superseded_changes(uuid)',
  'execute');

ROLLBACK;
