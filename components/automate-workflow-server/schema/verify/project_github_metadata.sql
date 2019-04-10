-- Verify project_github_metadata

BEGIN;

SELECT project_id,
       repo_name,
       repo_owner,
       token
  FROM project_github_metadata WHERE FALSE;

ROLLBACK;
