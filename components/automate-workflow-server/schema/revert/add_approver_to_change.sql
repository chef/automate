-- Revert add_approver_to_change

BEGIN;

ALTER TABLE changes DROP COLUMN approved_by;

COMMIT;
