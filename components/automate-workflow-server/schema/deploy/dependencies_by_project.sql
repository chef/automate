-- Deploy delivery:dependencies_by_project to pg

BEGIN;

DROP FUNCTION IF EXISTS dependencies_by_project(
  p_enterprise_name enterprises.name%TYPE,
  p_organization_name organizations.name%TYPE,
  p_project_name projects.name%TYPE);

CREATE OR REPLACE FUNCTION dependencies_by_project(
  p_enterprise_name enterprises.name%TYPE,
  p_organization_name organizations.name%TYPE,
  p_project_name projects.name%TYPE)
RETURNS TABLE(
    pipeline_id pipelines.id%TYPE,
    dependencies changesets.dependencies%TYPE,
    consumers BIGINT[])
LANGUAGE plpgsql
AS $$
DECLARE
    v_project_id projects.id%TYPE;
BEGIN
  SELECT project_id
  FROM to_ids(p_enterprise_name, NULL, p_organization_name, p_project_name, NULL)
  INTO v_project_id;

  RETURN QUERY WITH latest_changesets AS (
      SELECT DISTINCT ON (pipeline_id) cs.*
      FROM changesets AS cs
      WHERE cs.status = 'closed'
      ORDER BY cs.pipeline_id, cs.delivered_at DESC
  )
  SELECT pipe.id as pipeline_id,
         latest.dependencies as dependencies,
         ARRAY(SELECT l1.pipeline_id
               FROM latest_changesets AS l1
               WHERE pipe.id = ANY (l1.dependencies)) as consumers
  FROM projects AS proj
  JOIN pipelines AS pipe
  ON pipe.project_id = proj.id
  JOIN latest_changesets AS latest
  ON latest.pipeline_id = pipe.id
  WHERE proj.id = v_project_id;
END;
$$;

COMMENT ON FUNCTION dependencies_by_project(
  p_enterprise_name enterprises.name%TYPE,
  p_organization_name organizations.name%TYPE,
  p_project_name projects.name%TYPE) IS
$$
For a given project, find the dependencies and consumers for pipelines in that
that project.
$$;
COMMIT;
