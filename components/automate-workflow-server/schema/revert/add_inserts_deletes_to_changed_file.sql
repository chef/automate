-- Revert add_inserts_deletes_to_changed_file

BEGIN;

ALTER TABLE patchset_changed_files DROP COLUMN inserts;
ALTER TABLE patchset_changed_files DROP COLUMN deletes;

COMMIT;
