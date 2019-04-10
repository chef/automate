-- Deploy delivery:convert_to_githubV2 to pg
-- requires: project_github_metadata

BEGIN;

CREATE OR REPLACE FUNCTION convert_to_githubV2(
 p_project_id projects.id%TYPE,
 p_repo_owner project_github_metadata.repo_owner%TYPE,
 p_repo_name project_github_metadata.repo_name%TYPE
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
 INSERT INTO project_github_metadata(project_id, repo_owner, repo_name, token)
 VALUES (p_project_id, p_repo_owner, p_repo_name, 'unused');

 UPDATE projects
    SET scm_module = 'github_scm'
 WHERE id = p_project_id
 RETURNING projects.*
 INTO v_project;

 RETURN NEXT v_project;
END;
$$;

COMMIT;
