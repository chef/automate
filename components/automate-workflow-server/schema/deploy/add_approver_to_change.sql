-- Deploy add_approver_to_change

BEGIN;

ALTER TABLE changes ADD COLUMN approved_by TEXT;

COMMIT;
