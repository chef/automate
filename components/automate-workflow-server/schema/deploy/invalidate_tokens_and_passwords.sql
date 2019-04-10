-- Deploy invalidate_tokens_and_passwords

BEGIN;

CREATE TYPE credential AS ENUM('password', 'token');

CREATE OR REPLACE FUNCTION credential_table(p_credential credential)
RETURNS NAME
LANGUAGE SQL IMMUTABLE
AS $$
  SELECT CASE p_credential
         WHEN 'password' THEN 'user_passwords'::NAME
         WHEN 'token'    THEN 'user_tokens'::NAME
         END;
$$;

CREATE OR REPLACE FUNCTION invalidate(p_credential credential)
RETURNS VOID
LANGUAGE plpgsql
AS $$
BEGIN
  EXECUTE format('TRUNCATE %I', credential_table(p_credential));
END;
$$;

COMMENT ON FUNCTION invalidate(p_credential credential) IS
'Invalidate all tokens or passwords in the entire system; *every*
user, across *all* enterprises. In case of emergency, break this
glass.';

-- SELECT invalidate('token');
-- SELECT invalidate('password');

CREATE OR REPLACE FUNCTION invalidate(
  p_credential credential,
  p_enterprise_name enterprises.name%TYPE)
RETURNS VOID
LANGUAGE plpgsql
AS $$
BEGIN
  EXECUTE format('DELETE FROM %I AS c
                   USING users AS u,
                         enterprises AS e
                   WHERE e.name = $1
                     AND c.id = u.id
                     AND u.enterprise_id = e.id', credential_table(p_credential))
    USING p_enterprise_name;
END;
$$;

COMMENT ON FUNCTION invalidate(
  p_credential credential,
  p_enterprise_name enterprises.name%TYPE) IS
'Invalidate all tokens or passwords for all users in the given
enterprise only';

-- SELECT invalidate('token', 'my_enterprise');
-- SELECT invalidate('password', 'my_enterprise');

CREATE OR REPLACE FUNCTION invalidate(
  p_credential credential,
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE)
RETURNS VOID
LANGUAGE plpgsql
AS $$
BEGIN
  EXECUTE format('DELETE FROM %I AS c
                   USING users AS u,
                         enterprises AS e
                   WHERE e.name = $1
                     AND u.name = $2
                     AND c.id = u.id
                     AND u.enterprise_id = e.id', credential_table(p_credential))
    USING p_enterprise_name, p_user_name;
END;
$$;

COMMENT ON FUNCTION invalidate(
  p_credential credential,
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE) IS
'Invalidate all the tokens or the password of a specific user.';

-- SELECT invalidate('token', 'my_enterprise', 'me');
-- SELECT invalidate('password', 'my_enterprise', 'me');

COMMIT;
