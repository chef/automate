-- Revert get_changes

BEGIN;

DROP FUNCTION IF EXISTS get_changes(
  p_enterprise_name enterprises.name%TYPE,
  p_organization_name organizations.name%TYPE,
  p_project_name projects.name%TYPE,

  p_pipeline_name              pipelines.name%TYPE,
  p_state                      cd_patchset_status,
  p_last_change_id             changes.id%TYPE,
  p_reverse_chronological_sort BOOLEAN,
  p_limit                      INTEGER);

COMMIT;
