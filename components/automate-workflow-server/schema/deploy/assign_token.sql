-- Deploy assign_token

BEGIN;

--TODO: only returning SETOF TOKEN in order to play nice with sqerl (I
--think? Hard to distinguish between adding and not)
CREATE OR REPLACE FUNCTION assign_token(
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE,
  p_token user_tokens.auth_token%TYPE)
RETURNS SETOF user_tokens.auth_token%TYPE
LANGUAGE SQL
AS $$
  -- TODO: Any benefit to generating tokens in the database? Then it
  -- could just be 'assign_token(user_id), and we'd return the generated
  -- token. Rare prefix collisions could be handled here as well by
  -- retrying

  -- JOINing the enterprise to the user all the time could get
  -- cumbersome... wonder if it'd be worth making the FK the name so
  -- we can query directly
  INSERT INTO user_tokens(id, auth_token)
  SELECT u.id, p_token
  FROM users AS u
  JOIN enterprises AS e
    ON u.enterprise_id = e.id
  WHERE u.name = p_user_name
    AND e.name = p_enterprise_name
  RETURNING auth_token;
$$;

COMMIT;
