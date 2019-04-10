-- Revert delivery:remove_user_type_change_exception from pg

BEGIN;

CREATE OR REPLACE FUNCTION raise_user_type_change_exception()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
DECLARE
  user_name users.name%TYPE;
  enterprise_name enterprises.name%TYPE;
BEGIN
  SELECT u.name, e.name
  FROM users AS u
  JOIN enterprises AS e
    ON u.enterprise_id = e.id
  WHERE u.id = OLD.id
  INTO user_name, enterprise_name;

  RAISE EXCEPTION
  USING ERRCODE = 'CD002',
        MESSAGE = 'User type cannot be changed',
        DETAIL  = 'An attempt to change user "' || user_name ||
                  '" in enterprise "' || enterprise_name ||
                  '" from "' || OLD.user_type || '" to "' || NEW.user_type ||
                  '" was made. This is not allowed',
        HINT    = 'Don''t do that';
END;
$$;

CREATE TRIGGER prevent_type_switch
BEFORE UPDATE
ON users
FOR EACH ROW
WHEN (NEW.user_type != OLD.user_type)
EXECUTE PROCEDURE raise_user_type_change_exception();

COMMIT;
