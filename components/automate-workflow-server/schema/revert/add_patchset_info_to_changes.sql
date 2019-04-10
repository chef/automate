-- Revert add_patchset_info_to_changes

BEGIN;

ALTER TABLE changes DROP COLUMN latest_patchset_status;
ALTER TABLE changes DROP COLUMN latest_patchset;
ALTER TABLE changes DROP COLUMN submitted_at;
ALTER TABLE changes DROP COLUMN submitted_by;

COMMIT;
