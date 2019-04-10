-- Deploy delivery:convert_to_bb to pg
-- requires: project_bitbucket_metadata

BEGIN;

CREATE OR REPLACE FUNCTION convert_to_bb(
  p_project_id projects.id%TYPE,
  p_bitbucket_project project_bitbucket_metadata.bitbucket_project%TYPE,
  p_repo_name project_bitbucket_metadata.repo_name%TYPE
)
RETURNS SETOF projects
ROWS 1
LANGUAGE plpgsql
AS $$
DECLARE
  v_project projects%ROWTYPE;
BEGIN
  -- try to insert
  -- add the scm-specific metadata
  INSERT INTO project_bitbucket_metadata(project_id, bitbucket_project, repo_name)
  VALUES (p_project_id, p_bitbucket_project, p_repo_name);

  UPDATE projects
     SET scm_module = 'bitbucket_scm'
  WHERE id = p_project_id
  RETURNING projects.*
  INTO v_project;

  RETURN NEXT v_project;
END;
$$;

COMMIT;
