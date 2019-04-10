-- Revert delivery:add_dependencies_to_changesets from pg

BEGIN;

ALTER TABLE changesets DROP COLUMN dependencies;

COMMIT;
