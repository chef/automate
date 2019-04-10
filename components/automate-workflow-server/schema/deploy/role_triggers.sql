-- Deploy role_triggers

BEGIN;

CREATE TRIGGER ensure_membership
BEFORE INSERT OR UPDATE ON enterprise_user_roles
FOR EACH ROW
  EXECUTE PROCEDURE ensure_role_membership_prerequisite();

CREATE TRIGGER ensure_membership
BEFORE INSERT OR UPDATE ON organization_user_roles
FOR EACH ROW
  EXECUTE PROCEDURE ensure_role_membership_prerequisite();

COMMIT;
