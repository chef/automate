-- Revert add_changeset_fk_to_changes

BEGIN;

ALTER TABLE changes
DROP COLUMN changeset_id;

COMMIT;
