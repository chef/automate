-- Deploy delivery:remove_user_type_change_exception to pg

BEGIN;

DROP TRIGGER IF EXISTS prevent_type_switch
  ON users;

DROP FUNCTION IF EXISTS raise_user_type_change_exception();

COMMIT;
