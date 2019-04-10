-- Deploy project_and_pipeline_role_triggers
-- requires: ensure_role_membership_prerequisite

BEGIN;

CREATE TRIGGER ensure_membership
BEFORE INSERT OR UPDATE ON project_user_roles
FOR EACH ROW
  EXECUTE PROCEDURE ensure_role_membership_prerequisite();

CREATE TRIGGER ensure_membership
BEFORE INSERT OR UPDATE ON pipeline_user_roles
FOR EACH ROW
  EXECUTE PROCEDURE ensure_role_membership_prerequisite();

COMMIT;
