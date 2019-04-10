-- Deploy delivery:add_superseding_change_id_to_change to pg

BEGIN;

ALTER TABLE changes ADD COLUMN superseding_change_id UUID;

COMMIT;
