-- Revert project_and_pipeline_role_triggers

BEGIN;

DROP TRIGGER IF EXISTS ensure_membership
  ON project_user_roles;

DROP TRIGGER IF EXISTS ensure_membership
  ON pipeline_user_roles;

COMMIT;
