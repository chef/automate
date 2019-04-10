-- Deploy delivery:add_approved_at_to_change to pg
-- requires: changes

BEGIN;

ALTER TABLE changes ADD COLUMN approved_at cd_timestamp DEFAULT NULL;

COMMIT;
