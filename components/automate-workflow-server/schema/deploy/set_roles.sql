-- Deploy set_roles

BEGIN;

CREATE OR REPLACE FUNCTION set_roles(
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE,
  p_organization_name organizations.name%TYPE,
  p_project_name projects.name%TYPE,
  p_pipeline_name pipelines.name%TYPE,
  p_roles text[]
)
RETURNS TABLE(role delivery_role, scopes text[])
LANGUAGE plpgsql
AS $$
DECLARE

  to_revoke text[];

BEGIN

  -- So, this is kinda slick. "Setting" roles means "the user should
  -- have these roles, and ONLY these roles". We could just nuke all
  -- the user's existing role grants at the scope, and then insert new
  -- ones.
  --
  -- OR
  --
  -- We could revoke the ones not in the list (comparing to the list
  -- of all possible roles) and then grant the ones in the list. No
  -- database row churn that way, and we rely on previously written
  -- code paths.
  --
  -- Alternatively, we could re-implement the logic of revoke and
  -- grant in such a way as to share the ID lookups that both
  -- functions currently perform. However, I'm not clear that that's
  -- going to be an enormous strain on resources, so we can cross that
  -- bridge if we ever find out that bridge actually exists.

  SELECT array_agg(a)
  FROM (
    SELECT unnest(enum_range(NULL::delivery_role)::text[]) AS a
    EXCEPT
    SELECT unnest(p_roles) AS a) AS i
  INTO to_revoke;

  PERFORM revoke_roles(p_enterprise_name,
                       p_user_name,
                       p_organization_name,
                       p_project_name,
                       p_pipeline_name,
                       to_revoke);

   -- Since `grant_roles` returns the scoped roles, we'll just return
   -- what it returns.
   --
   -- This implementation REALLY suggests re-working the authz
   -- procedures to have external and internal interfaces. As it is,
   -- we'll be converting names to IDs FOUR TIMES when we run
   -- `set_roles`, and will be running `scoped_roles` TWICE.
   RETURN QUERY SELECT * FROM  grant_roles(p_enterprise_name,
                                           p_user_name,
                                           p_organization_name,
                                           p_project_name,
                                           p_pipeline_name,
                                           p_roles);
END;
$$;

COMMIT;
