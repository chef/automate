-- Revert default_changesets

BEGIN;

  UPDATE changes
  SET changeset_id = NULL,
  delivered_by = NULL,
  delivered_at = NULL
  FROM changesets
  WHERE changes.changeset_id = changesets.id
  AND changesets.delivered_by = 'pre_changeset_change';

  DELETE FROM changesets
  WHERE delivered_by = 'pre_changeset_change';

COMMIT;
