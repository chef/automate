-- Deploy scoped_roles

BEGIN;

CREATE OR REPLACE FUNCTION scoped_roles(
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE,
  p_organization_name organizations.name%TYPE,
  p_project_name projects.name%TYPE,
  p_pipeline_name pipelines.name%TYPE
)
RETURNS TABLE(role delivery_role, scopes text[])
LANGUAGE plpgsql
AS $$
DECLARE
  v_enterprise_id enterprises.id%TYPE;
  v_user_id users.id%TYPE;

  -- These are allowed to be NULL, since this function handles
  -- returning the roles at any scope; just explicitly declaring that,
  -- even though it's the default
  v_organization_id organizations.id%TYPE = NULL;
  v_project_id projects.id%TYPE = NULL;
  v_pipeline_id pipelines.id%TYPE = NULL;

BEGIN

  SELECT enterprise_id, user_id, organization_id, project_id, pipeline_id
  FROM to_ids(p_enterprise_name, p_user_name, p_organization_name, p_project_name, p_pipeline_name)
  INTO v_enterprise_id, v_user_id, v_organization_id, v_project_id, v_pipeline_id;

  -- Now we can simply query the authorization tables for the roles
  -- granted and UNION everything together; if any identifier is NULL,
  -- the query at that scope will return nothing, which is what we
  -- want. Then we simply group and aggregate to get a structure like
  -- this:
  --
  --    role    |                   scopes
  -- -----------+--------------------------------------------
  --  admin     | {enterprise,organization,project,pipeline}
  --  committer | {project}
  --  observer  | {pipeline}
  --
  -- Note that both roles and scopes are sorted in enum order, which
  -- is perhaps not so meaningful for roles, but is nicely
  -- hierarchical for the scopes.
  --
  -- Also note how much simpler the queries are using the IDs instead
  -- of the names... think of the joins and the repetition!
  --
  -- One last note! Even though we have an array of 'delivery_scope'
  -- values, we cast to a text array for the final result. Apparently,
  -- the Erlang postgres driver knows how to render individual enums
  -- as text (which is why we don't have to cast 'role' to text here),
  -- but arrays of enum values cause issues. Specifically, we'll just
  -- get the array literal as a string, like
  --
  --   '{enterprise, organization}'
  --
  -- instead of an actual list. Boo, hiss. Oh well.
  RETURN QUERY SELECT s.role, array_agg(s.scope ORDER BY s.scope)::text[]
  FROM (
    SELECT r.role, 'enterprise'::delivery_scope AS scope
    FROM enterprise_user_roles AS r
    WHERE enterprise_id = v_enterprise_id
      AND user_id = v_user_id

    UNION

    SELECT r.role, 'organization'::delivery_scope AS scope
    FROM organization_user_roles AS r
    WHERE organization_id = v_organization_id
      AND user_id = v_user_id

    UNION

    SELECT r.role, 'project'::delivery_scope AS scope
    FROM project_user_roles AS r
    WHERE project_id = v_project_id
      AND user_id = v_user_id

    UNION

    SELECT r.role, 'pipeline'::delivery_scope AS scope
    FROM pipeline_user_roles AS r
    WHERE pipeline_id = v_pipeline_id
      AND user_id = v_user_id
  ) AS s
  GROUP BY s.role
  ORDER BY s.role;

END;
$$;

COMMIT;
