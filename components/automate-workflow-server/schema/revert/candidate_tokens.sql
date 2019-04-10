-- Revert candidate_tokens

BEGIN;

DROP FUNCTION IF EXISTS candidate_tokens(
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE,
  p_token user_tokens.auth_token%TYPE);

COMMIT;
