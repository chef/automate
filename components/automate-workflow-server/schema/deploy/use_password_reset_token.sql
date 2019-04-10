-- Deploy delivery:use_password_reset_token to pg
-- requires: password_reset_tokens

BEGIN;

CREATE OR REPLACE FUNCTION use_password_reset_token(
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE,
  p_token password_reset_tokens.token%TYPE)
RETURNS SETOF password_reset_tokens.token%TYPE
LANGUAGE SQL
AS $$
  DELETE FROM password_reset_tokens AS t
   USING users AS u,
         enterprises AS e
   WHERE u.enterprise_id = e.id
     AND t.user_id = u.id
     AND e.name = p_enterprise_name
     AND u.name = p_user_name
     AND EXISTS (SELECT *
                   FROM password_reset_tokens AS t,
                        users AS u,
                        enterprises AS e
                  WHERE u.enterprise_id = e.id
                    AND t.user_id = u.id
                    AND e.name = p_enterprise_name
                    AND u.name = p_user_name
                    AND t.token = p_token
                    AND t.expiry > NOW())
  RETURNING t.token;
$$;

COMMENT ON FUNCTION use_password_reset_token(
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE,
  p_token password_reset_tokens.token%TYPE) IS

'Uses the given password reset token to set a new password for the user.
If the given token exists, and has not been expired, all reset tokens for
this user are removed, and returned. Returns NULL if either the enterprise,
user, or specified token don''t exist';

COMMIT;
