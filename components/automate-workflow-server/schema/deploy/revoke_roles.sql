-- Deploy revoke_roles

BEGIN;


CREATE OR REPLACE FUNCTION revoke_roles(
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
  -- This is the scope at which we are granting roles. It is
  -- determined by how much information is passed into this function
  v_scope delivery_scope;

  v_enterprise_id enterprises.id%TYPE;
  v_user_id users.id%TYPE;

  -- These are allowed to be NULL, since this function handles
  -- revoking roles at any scope; just explicitly declaring that,
  -- even though it's the default
  v_organization_id organizations.id%TYPE = NULL;
  v_project_id projects.id%TYPE = NULL;
  v_pipeline_id pipelines.id%TYPE = NULL;

  -- In order to easily handle deletes from our different role tables
  -- without having to do a bunch of copy-paste, we're going to use
  -- some dynamic SQL. In order to do that, however, we'll need some
  -- variables for certain bits of data that we'll be computing
  -- on-the-fly.
  v_table NAME;
  v_id_column NAME;
  v_id_value BIGINT;
BEGIN

  -- Convert everything to IDs for simpler queries. Also determine
  -- what scope we're operating at
  SELECT * FROM to_ids(p_enterprise_name, p_user_name, p_organization_name, p_project_name, p_pipeline_name)
  INTO v_scope, v_enterprise_id, v_user_id, v_organization_id, v_project_id, v_pipeline_id;

  -- Based on the scope we're at, determine what tables, columns, and
  -- values we'll be working with
  v_table = quote_ident(v_scope || '_user_roles');
  v_id_column = quote_ident(v_scope || '_id');
  v_id_value = CASE v_scope
               WHEN 'enterprise'   THEN v_enterprise_id
               WHEN 'organization' THEN v_organization_id
               WHEN 'project'      THEN v_project_id
               WHEN 'pipeline'     THEN v_pipeline_id
               END;

  -- If all this looks familiar, it's because you saw something very
  -- similar in grant_roles.
  EXECUTE format(
    'WITH to_delete AS (
     SELECT unnest(%L::delivery_role[]) AS role
     INTERSECT
     SELECT role
     FROM %I
     WHERE %I = %s
       AND user_id = %s
     )
     DELETE FROM %I AS r
     USING to_delete AS d
     WHERE r.%I      = %s
       AND r.user_id = %s
       AND r.role    = d.role',
     p_roles,
     v_table,
     v_id_column, v_id_value,
     v_user_id,
     v_table,
     v_id_column, v_id_value,
     v_user_id);

   -- We want to show the result of the previous operation, so we
   -- return the current roles at this scope. The current
   -- implementation is a bit lacking, in that scoped_roles is going
   -- to convert the names into IDs again, which is wasteful (but that
   -- stuff is still going to be in the cache, so not too bad).
   --
   -- In the future, we should probably re-work these authz procedures
   -- to have a "name" interface for the application to use, and then
   -- an "id" interface for internal use.
   RETURN QUERY SELECT * FROM scoped_roles(p_enterprise_name,
                                           p_user_name,
                                           p_organization_name,
                                           p_project_name,
                                           p_pipeline_name);

END;
$$;

COMMIT;
