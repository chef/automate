-- Revert revoke_token

BEGIN;

DROP FUNCTION IF EXISTS revoke_token(
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE,
  p_token user_tokens.auth_token%TYPE);

COMMIT;
