-- Deploy delivery:dependencies_by_project to pg

BEGIN;

CREATE OR REPLACE FUNCTION dependencies_by_project(
  p_enterprise_name enterprises.name%TYPE,
  p_organization_name organizations.name%TYPE,
  p_project_name projects.name%TYPE)
RETURNS TABLE(
    dependencies changesets.dependencies%TYPE,
    consumers BIGINT[])
LANGUAGE plpgsql
AS $$
DECLARE
    v_pipeline_id pipelines.id%TYPE;
    v_dependencies changesets.dependencies%TYPE;
    v_consumers BIGINT[];
BEGIN
  -- Currently we only return dependencies based on the
  -- master pipeline. This will need to be modified to
  -- handle arbitrary pipelines.
  SELECT pipeline_id
  FROM to_ids(p_enterprise_name, NULL, p_organization_name, p_project_name, 'master')
  INTO v_pipeline_id;

  SELECT cs_deps.dependencies
  FROM changesets AS cs_deps
  WHERE cs_deps.pipeline_id = v_pipeline_id
  AND cs_deps.status='closed'
  ORDER BY cs_deps.delivered_at DESC
  LIMIT 1
  INTO v_dependencies;

  WITH latest_changesets AS (
    SELECT DISTINCT ON (pipeline_id) cs.*
    FROM changesets AS cs
    WHERE cs.status='closed'
    ORDER BY cs.pipeline_id, cs.delivered_at DESC
  )
  SELECT INTO v_consumers ARRAY(SELECT latest.pipeline_id
  FROM latest_changesets AS latest
  WHERE v_pipeline_id = ANY (latest.dependencies));

  RETURN QUERY VALUES (v_dependencies, v_consumers);
END;
$$;

COMMENT ON FUNCTION dependencies_by_project(
  p_enterprise_name enterprises.name%TYPE,
  p_organization_name organizations.name%TYPE,
  p_project_name projects.name%TYPE) IS
$$
For a given project, find the dependencies and consumers of that project in union. 
$$;
COMMIT;
