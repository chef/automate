-- Deploy delivery:rename_table_change_bitbucket_metadata to pg
-- requires: change_bitbucket_metadata
-- requires: convert_to_local

BEGIN;

ALTER TABLE change_bitbucket_metadata RENAME TO scm_changes;

CREATE OR REPLACE FUNCTION convert_to_local(p_project_id projects.id%TYPE)
RETURNS SETOF projects
ROWS 1
LANGUAGE plpgsql
AS $$
DECLARE
  v_project projects%ROWTYPE;
BEGIN

  -- Delete metadata associated to the project
  DELETE FROM project_bitbucket_metadata
  WHERE project_id = p_project_id;

  -- Delete metadata associated to all changes within the project
  DELETE FROM scm_changes
  WHERE change_id IN (
    SELECT id
    FROM changes
    WHERE pipeline_id IN (
      SELECT id
      FROM pipelines
      WHERE project_id = p_project_id));

  -- Update project's scm_module
  UPDATE projects
     SET scm_module = 'deliv_scm_local'
   WHERE id = p_project_id
   RETURNING projects.*
   INTO v_project;

  RETURN NEXT v_project;

END;
$$;


COMMIT;
