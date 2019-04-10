-- Deploy delivery:dependencies_by_project to pg

BEGIN;

DROP FUNCTION dependencies_by_project(text, text, text);

CREATE OR REPLACE FUNCTION dependencies_by_project(
  p_enterprise_name enterprises.name%TYPE,
  p_organization_name organizations.name%TYPE,
  p_project_name projects.name%TYPE)
RETURNS changesets.dependencies%TYPE
LANGUAGE plpgsql
AS $$
DECLARE
    v_pipeline_id pipelines.id%TYPE;
    v_dependencies changesets.dependencies%TYPE;
BEGIN
  -- Currently we only return dependencies based on the
  -- master pipeline. This will need to be modified to
  -- handle arbitrary pipelines.
  SELECT pipeline_id
  FROM to_ids(p_enterprise_name, NULL, p_organization_name, p_project_name, 'master')
  INTO v_pipeline_id;

  SELECT dependencies
  FROM changesets
  WHERE pipeline_id=v_pipeline_id
  INTO v_dependencies;

  RETURN v_dependencies;
END;
$$;
COMMIT;
