-- Verify add_to_changeset

BEGIN;

SELECT has_function_privilege(
  'add_to_changeset(uuid)',
  'execute');

ROLLBACK;
