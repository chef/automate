-- Deploy add_changeset_fk_to_changes
-- requires: changes
-- requires: changesets

BEGIN;

ALTER TABLE changes
ADD COLUMN changeset_id UUID REFERENCES changesets(id)
ON DELETE RESTRICT;

COMMIT;
