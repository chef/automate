-- Deploy add_inserts_deletes_to_changed_file

BEGIN;

ALTER TABLE patchset_changed_files ADD COLUMN inserts TEXT;
ALTER TABLE patchset_changed_files ADD COLUMN deletes TEXT;

COMMIT;
