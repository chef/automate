-- Deploy candidate_tokens

BEGIN;

CREATE OR REPLACE FUNCTION candidate_tokens(
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE,
  p_token user_tokens.auth_token%TYPE)
RETURNS TABLE(token user_tokens.auth_token%TYPE,
              birthday user_tokens.auth_token_bday%TYPE)
LANGUAGE plpgsql STABLE
AS $$
BEGIN
  RETURN QUERY SELECT t.auth_token, t.auth_token_bday
  FROM users AS u
  JOIN enterprises AS e
    ON u.enterprise_id = e.id
  LEFT JOIN user_tokens AS t
    ON u.id = t.id
  WHERE u.name = p_user_name
    AND e.name = p_enterprise_name
    AND t.auth_token::token_prefix = p_token::token_prefix;
END;
$$;

COMMENT ON FUNCTION candidate_tokens(
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE,
  p_token user_tokens.auth_token%TYPE) IS

'Selects tokens from the user''s token space based on the prefix of
the supplied token. Matches are performed based on a prefix rather
than on the entire token as a guard against potential timing-based
attacks. The application receives the candidate token(s) and can then
perform a constant-time comparison between the supplied token and the
retrieved candidate.

Part of validating a token involves comparing its birthday with an
action-specific TTL, so that is returned by this function as well.

Often, this function will just return the same token it was passed,
but again, it depends only on the prefix. If the given token is, say,
"chef_boyardee", this function would also return "chef_is_awesome" if
the latter was stored in the database.

The length of the prefix is determined by the `token_prefix`
domain. Applications should call this function to retrieve the
candidate token rather than simply executing the query this function
perform; this insulates the application from future prefix length
changes, and centralizes knowledge of the prefix length solely in the
database.

On the off chance that a user will have multiple tokens that share a
prefix, this function can return multiple rows; the application should
check each one until a solid match is found.';

COMMIT;
