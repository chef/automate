-- Deploy upsert_project_github_metadata
-- requires: project_github_metadata

BEGIN;

DROP FUNCTION IF EXISTS upsert_project_github_metadata(
  p_project_id project_github_metadata.project_id%TYPE,
  p_repo_name project_github_metadata.repo_name%TYPE,
  p_repo_owner project_github_metadata.repo_owner%TYPE,
  p_token project_github_metadata.token%TYPE
);

COMMIT;
