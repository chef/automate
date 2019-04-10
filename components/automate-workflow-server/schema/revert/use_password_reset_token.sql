-- Revert delivery:use_password_reset_token from pg

BEGIN;

DROP FUNCTION IF EXISTS use_password_reset_token(
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE,
  p_token password_reset_tokens.token%TYPE);

COMMIT;
