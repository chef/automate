-- Deploy delete_internal_user

BEGIN;

CREATE OR REPLACE FUNCTION delete_internal_user()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
BEGIN
  DELETE FROM users WHERE id = OLD.id;
  IF FOUND THEN
    RETURN OLD;
  ELSE
    RETURN NULL;
  END IF;
END;
$$;

COMMIT;
