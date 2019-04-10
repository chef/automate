-- Revert delivery:add_superseding_change_id_to_change from pg

BEGIN;

ALTER TABLE changes DROP COLUMN superseding_change_id;

COMMIT;
