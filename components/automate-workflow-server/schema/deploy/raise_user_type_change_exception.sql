-- Deploy raise_user_type_change_exception

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

COMMIT;
