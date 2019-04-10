-- Revert delivery:create_bitbucket_scm_project from pg

BEGIN;

DROP FUNCTION IF EXISTS create_bitbucket_scm_project(
  p_enterprise_name enterprises.name%TYPE,
  p_organization_name organizations.name%TYPE,
  p_project_name projects.name%TYPE,
  p_pipeline_name pipelines.name%TYPE,
  p_scm_module projects.scm_module%TYPE,
  p_bitbucket_project project_bitbucket_metadata.bitbucket_project%TYPE,
  p_repo_name project_bitbucket_metadata.repo_name%TYPE
);

COMMIT;
