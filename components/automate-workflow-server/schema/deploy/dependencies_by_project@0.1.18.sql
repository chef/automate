-- Deploy delivery:dependencies_by_project to pg

BEGIN;

DROP FUNCTION dependencies_by_project(text, text, text);

CREATE OR REPLACE FUNCTION dependencies_by_project(
  p_enterprise_name enterprises.name%TYPE,
  p_organization_name organizations.name%TYPE,
  p_project_name projects.name%TYPE)
RETURNS TABLE(
    dependencies changesets.dependencies%TYPE,
    consumers BIGINT[])
LANGUAGE plpgsql
ROWS 1
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

  -- TODO: Only select from latest changeset
  SELECT cs_deps.dependencies
  FROM changesets AS cs_deps
  WHERE cs_deps.pipeline_id=v_pipeline_id
  INTO v_dependencies;

  SELECT INTO v_consumers ARRAY(SELECT cs_reqs.pipeline_id
  FROM changesets AS cs_reqs
  WHERE v_pipeline_id = ANY (cs_reqs.dependencies));

  RETURN QUERY VALUES (v_dependencies, v_consumers);
END;
$$;
COMMIT;
