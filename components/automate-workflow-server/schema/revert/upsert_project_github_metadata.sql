-- Deploy upsert_project_github_metadata
-- requires: project_github_metadata

-- Most of this procedure could be replaced with a single UPSERT call when it
-- rolls around in PGSQL
-- (see http://www.craigkerstiens.com/2015/05/08/upsert-lands-in-postgres-9.5/)

BEGIN;

CREATE OR REPLACE FUNCTION upsert_project_github_metadata(
  p_project_id project_github_metadata.project_id%TYPE,
  p_repo_owner project_github_metadata.repo_owner%TYPE,
  p_repo_name project_github_metadata.repo_name%TYPE,
  p_token project_github_metadata.token%TYPE
)
RETURNS SETOF project_github_metadata
ROWS 1
LANGUAGE plpgsql
AS $$
DECLARE
  v_other_project_id projects.id%TYPE;
BEGIN
  -- try to insert
  RETURN QUERY
  INSERT INTO project_github_metadata(project_id, repo_name, repo_owner, token)
  VALUES (p_project_id, p_repo_name, p_repo_owner, p_token)
  RETURNING project_github_metadata.*;
EXCEPTION
  WHEN unique_violation THEN
    -- we could pattern match on SQLERRM to know if the violation is due to
    -- the primary key or to the unique constraint on github repos, but that
    -- would seem rather brittle
    SELECT project_id
      FROM project_github_metadata
     WHERE repo_owner = p_repo_owner
       AND repo_name = p_repo_name
       AND project_id != p_project_id
      INTO v_other_project_id;

    IF FOUND THEN
      RAISE EXCEPTION
        USING ERRCODE = 'CD019',
              MESSAGE = 'This Github repo is already linked to another Delivery project',
              DETAIL  = 'This Github repo is already linked to project ' || v_other_project_id,
              HINT    = 'A given Github repo can be linked to at most one Delivery project';
    ELSE
      -- the unique_violation was due to the primary key, simply update
      RETURN QUERY
      UPDATE project_github_metadata
         SET repo_owner = p_repo_owner,
             repo_name = p_repo_name,
             token = p_token
       WHERE project_id = p_project_id
   RETURNING project_github_metadata.*;
    END IF;
END;
$$;

COMMIT;
