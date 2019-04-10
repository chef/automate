-- Deploy delivery:create_bitbucket_scm_project to pg
-- requires: project_bitbucket_metadata

BEGIN;

CREATE OR REPLACE FUNCTION create_bitbucket_scm_project(
  p_enterprise_name enterprises.name%TYPE,
  p_organization_name organizations.name%TYPE,
  p_project_name projects.name%TYPE,
  p_pipeline_name pipelines.name%TYPE,
  p_scm_module projects.scm_module%TYPE,
  p_bitbucket_project project_bitbucket_metadata.bitbucket_project%TYPE,
  p_repo_name project_bitbucket_metadata.repo_name%TYPE
)
RETURNS SETOF projects
LANGUAGE plpgsql
ROWS 1
AS $$
DECLARE
  v_organization_id organizations.id%TYPE;
  v_project projects%ROWTYPE;
BEGIN
  SELECT organization_id
  FROM to_ids(p_enterprise_name, NULL, p_organization_name, NULL, NULL)
  INTO v_organization_id;

  -- create project
  INSERT INTO projects(organization_id, name, scm_module)
  VALUES (v_organization_id, p_project_name, p_scm_module)
  RETURNING projects.*
  INTO v_project;

  -- create pipeline
  INSERT INTO pipelines(project_id, name)
  VALUES (v_project.id, p_pipeline_name);

  -- add the scm-specific metadata
  INSERT INTO project_bitbucket_metadata(project_id, bitbucket_project, repo_name)
  VALUES (v_project.id, p_bitbucket_project, p_repo_name);

  RETURN NEXT v_project;
END;
$$;

COMMIT;
