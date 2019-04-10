-- Deploy prevent_type_switch_trigger

BEGIN;

CREATE TRIGGER prevent_type_switch
BEFORE UPDATE
ON users
FOR EACH ROW
WHEN (NEW.user_type != OLD.user_type)
EXECUTE PROCEDURE raise_user_type_change_exception();

COMMIT;
