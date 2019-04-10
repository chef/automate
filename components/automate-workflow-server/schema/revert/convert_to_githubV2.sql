-- Revert delivery:convert_to_githubV2 from pg

BEGIN;

DROP FUNCTION IF EXISTS convert_to_githubV2(
  p_project_id projects.id%TYPE,
  p_repo_owner project_github_metadata.repo_owner%TYPE,
  p_repo_name project_github_metadata.repo_name%TYPE
);


COMMIT;
