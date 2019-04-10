-- Revert prevent_type_switch_trigger

BEGIN;

DROP TRIGGER IF EXISTS prevent_type_switch
  ON users;

COMMIT;
