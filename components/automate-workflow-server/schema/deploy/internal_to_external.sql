-- Deploy internal_to_external

BEGIN;

CREATE OR REPLACE FUNCTION utility.internal_to_external(
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE)
RETURNS VOID
LANGUAGE plpgsql
AS $$
DECLARE
  v_user_id users.id%TYPE;
BEGIN

  IF p_user_name = 'admin' OR p_user_name = 'builder' THEN
    RAISE EXCEPTION
    USING MESSAGE = 'Cannot change user type for special users',
          DETAIL  = 'The user "' || p_user_name || '" MUST be an internal user!',
          HINT    = 'Don''t do that!';
  END IF;

  SELECT user_id FROM to_ids(p_enterprise_name, p_user_name, NULL, NULL, NULL)
  INTO v_user_id;

  UPDATE users SET user_type = 'external' WHERE id = v_user_id;
  DELETE FROM user_passwords WHERE id = v_user_id;
END;
$$;

COMMENT ON FUNCTION utility.internal_to_external(
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE)
IS
$$Convert an internally-defined user into an externally-defined one.$$;

COMMIT;
