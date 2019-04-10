-- Deploy to_ids

BEGIN;

CREATE OR REPLACE FUNCTION to_ids(
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE,
  p_organization_name organizations.name%TYPE,
  p_project_name projects.name%TYPE,
  p_pipeline_name pipelines.name%TYPE)
RETURNS TABLE(scope delivery_scope,
              enterprise_id enterprises.id%TYPE,
              user_id users.id%TYPE,
              organization_id organizations.id%TYPE,
              project_id projects.id%TYPE,
              pipeline_id pipelines.id%TYPE)
LANGUAGE plpgsql
AS $$
DECLARE
  v_enterprise_id enterprises.id%TYPE;

  -- These are allowed to be NULL, since this function handles
  -- id-mapping at any scope; just explicitly declaring that,
  -- even though it's the default
  v_user_id users.id%TYPE = NULL;
  v_organization_id organizations.id%TYPE = NULL;
  v_project_id projects.id%TYPE = NULL;
  v_pipeline_id pipelines.id%TYPE = NULL;

  -- It will be useful to determine what scope we're operating at
  -- based on the parameters passed. We'll assume we start at
  -- 'enterprise', and will progressively refine it as we move down
  -- the scope hierarchy and keep finding things.
  --
  -- We'll pass this back out to the caller; it's not *directly*
  -- involved with mapping names to IDs, but callers are going to need
  -- this information anyway, so we might as well get that information
  -- here, where it's really easy to determine.
  v_scope delivery_scope DEFAULT 'enterprise';
BEGIN

 -- Since it's going to be a common task to take a bunch of entity
 -- names from across the scope hierarchy and convert them into IDs,
 -- we'll just do it all at once in this function. That way we can
 -- consolidate all the conversion in one place, do it as efficiently
 -- as possible, and generate useful error messages when things go
 -- south.
 --
 -- It's declared as returning a TABLE, but it will only ever be one
 -- row "tall". That way, you can just do a big SELECT ... INTO
 -- statement and set a whole mess of variables at once.

  SELECT id
  FROM enterprises
  WHERE name = p_enterprise_name
  INTO v_enterprise_id;

  IF NOT FOUND THEN
    RAISE EXCEPTION
    USING ERRCODE = 'CD003',
          MESSAGE = 'Enterprise not found',
          DETAIL  = 'Enterprise "' || p_enterprise_name || '" not found',
          HINT    = 'Make sure the enterprise exists, and the name is spelled correctly';
  END IF;

  IF p_user_name IS NOT NULL THEN
    SELECT id
    FROM users AS u
    WHERE name = p_user_name
      AND u.enterprise_id = v_enterprise_id
    INTO v_user_id;

    IF NOT FOUND THEN
      RAISE EXCEPTION
      USING ERRCODE = 'CD004',
            MESSAGE = 'User not found',
            DETAIL  = 'User "' || p_user_name || '" not found in enterprise "' ||
                      p_enterprise_name || '"',
            HINT    = 'Make sure the user exists, and the name is spelled correctly';
    END IF;
  END IF;

  -- There is a strict dependency chain here; for a pipeline name to
  -- be non-NULL, both the project name and organization name should
  -- be non-NULL as well, for example. If any of this doesn't hold, a
  -- helpful exception will be thrown
  IF p_organization_name IS NOT NULL THEN
    SELECT id
    FROM organizations AS o
    WHERE name = p_organization_name
      AND o.enterprise_id = v_enterprise_id
    INTO v_organization_id;

    IF NOT FOUND THEN
      RAISE EXCEPTION
      USING ERRCODE = 'CD005',
            MESSAGE = 'Organization not found',
            DETAIL  = 'Organization "' || p_organization_name || '" not found in enterprise "' ||
                      p_enterprise_name || '"',
            HINT    = 'Make sure the organization exists, and the name is spelled correctly';
    END IF;

    -- Ah, we actually have a real organization, so we're at least
    -- operating at that scope
    v_scope = 'organization';

    IF p_project_name IS NOT NULL THEN
      SELECT id
      FROM projects AS p
      WHERE name = p_project_name
        AND p.organization_id = v_organization_id
      INTO v_project_id;

      IF NOT FOUND THEN
        RAISE EXCEPTION
        USING ERRCODE = 'CD006',
              MESSAGE = 'Project not found',
              DETAIL  = 'Project "' || p_project_name || '" was not found in organization "' ||
                        p_organization_name || '" in enterprise "' || p_enterprise_name || '"',
              HINT    = 'Make sure the project exists, and the name is spelled correctly';
      END IF;

      -- We have a project now; update the scope!
      v_scope = 'project';

      IF p_pipeline_name IS NOT NULL THEN
        SELECT id
        FROM pipelines AS p
        WHERE name = p_pipeline_name
          AND p.project_id = v_project_id
        INTO v_pipeline_id;

        IF NOT FOUND THEN
          RAISE EXCEPTION
          USING ERRCODE = 'CD007',
                MESSAGE = 'Pipeline not found',
                DETAIL  = 'Pipeline "' || p_pipeline_name || '" was not found in project "' ||
                          p_project_name || '" in organization "' || p_organization_name ||
                          '" in enterprise "' || p_enterprise_name || '"',
                HINT    = 'Make sure the pipeline exists, and the name is spelled correctly';
        END IF;

        -- Congratulations, you've made it all the way to the bottom
        -- of the scope hierarchy! Have a cookie!
        v_scope = 'pipeline';

      END IF; -- pipeline
    END IF; -- project
  END IF; -- organization

  -- Yup... all that work for *this*
  RETURN QUERY SELECT v_scope,
                      v_enterprise_id,
                      v_user_id,
                      v_organization_id,
                      v_project_id,
                      v_pipeline_id;
END;
$$;

COMMIT;
