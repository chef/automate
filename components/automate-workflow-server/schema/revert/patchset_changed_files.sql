-- Revert patchset_changed_files

BEGIN;

DROP TABLE IF EXISTS patchset_changed_files;

COMMIT;
