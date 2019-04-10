-- Verify delivery:changes_for_changeset_ids on pg

BEGIN;

SELECT has_function_privilege(
  'changes_for_changeset_ids(uuid[])',
  'execute');

END;
