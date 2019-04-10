-- Revert delivery:convert_to_bb from pg

BEGIN;

DROP FUNCTION IF EXISTS convert_to_bb(
  p_project_id projects.id%TYPE,
  p_bitbucket_project project_bitbucket_metadata.bitbucket_project%TYPE,
  p_repo_name project_bitbucket_metadata.repo_name%TYPE
);

COMMIT;
