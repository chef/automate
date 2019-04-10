-- Revert create_patchset_and_change

BEGIN;

DROP FUNCTION IF EXISTS create_patchset_and_change(
  p_enterprise_name enterprises.name%TYPE,
  p_submitter_name users.name%TYPE,
  p_organization_name organizations.name%TYPE,
  p_project_name projects.name%TYPE,
  p_pipeline_name pipelines.name%TYPE,
  p_feature_branch_name changes.feature_branch%TYPE,
  p_sha patchsets.sha%TYPE
);

COMMIT;
