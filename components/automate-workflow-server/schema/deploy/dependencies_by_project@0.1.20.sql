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
  JOIN changes AS ch
    ON cs_deps.id = ch.changeset_id
  JOIN scoped_stage_runs AS ssr
    ON ssr.change_id = ch.id
  WHERE ssr.pipeline_id=v_pipeline_id
  AND ssr.stage='union'
  ORDER BY ssr.id DESC
  LIMIT 1
  INTO v_dependencies;

  SELECT INTO v_consumers ARRAY(SELECT DISTINCT cs_reqs.pipeline_id
    FROM changesets AS cs_reqs
    JOIN changes AS ch
      ON cs_reqs.id = ch.changeset_id
    JOIN scoped_stage_runs AS ssr
      ON ssr.change_id = ch.id
    WHERE ssr.stage = 'union'
    AND v_pipeline_id = ANY (cs_reqs.dependencies));

  RETURN QUERY VALUES (v_dependencies, v_consumers);
END;
$$;
COMMIT;
