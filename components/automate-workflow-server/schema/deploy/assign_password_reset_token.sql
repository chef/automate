-- Deploy delivery:assign_password_reset_token to pg
-- requires: password_reset_tokens

BEGIN;

CREATE OR REPLACE FUNCTION assign_password_reset_token(
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE,
  p_token password_reset_tokens.token%TYPE,
  p_expiry INTERVAL)
RETURNS password_reset_tokens.token%TYPE
LANGUAGE SQL
AS $$
  INSERT INTO password_reset_tokens(user_id, token, expiry)
  SELECT u.id, p_token, NOW() + p_expiry
  FROM users AS u
  JOIN enterprises AS e
    ON u.enterprise_id = e.id
  WHERE u.name = p_user_name
    AND e.name = p_enterprise_name
  RETURNING token;
$$;

COMMIT;
