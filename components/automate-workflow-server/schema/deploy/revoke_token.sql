-- Deploy revoke_token

BEGIN;

CREATE OR REPLACE FUNCTION revoke_token(
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE,
  p_token user_tokens.auth_token%TYPE)
RETURNS user_tokens.auth_token%TYPE
LANGUAGE SQL
AS $$
  DELETE FROM user_tokens AS t
   USING users AS u,
         enterprises AS e
   WHERE u.enterprise_id = e.id
     AND t.id = u.id
     AND e.name = p_enterprise_name
     AND u.name = p_user_name
     AND t.auth_token = p_token
  RETURNING auth_token;
$$;

COMMENT ON FUNCTION revoke_token(
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE,
  p_token user_tokens.auth_token%TYPE) IS

'Revokes the given token from a user''s account. Returns the token
that was revoked, or NULL if either the enterprise, user, or specified
token don''t exist';

COMMIT;
