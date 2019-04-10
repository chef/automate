-- Revert delivery:add_approved_at_to_change from pg

BEGIN;

ALTER TABLE changes DROP COLUMN approved_at;

COMMIT;
