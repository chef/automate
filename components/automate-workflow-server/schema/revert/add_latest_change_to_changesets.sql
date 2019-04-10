-- Revert delivery:add_latest_change_to_changesets from pg

BEGIN;

ALTER TABLE changesets DROP COLUMN latest_change_id;

COMMIT;
