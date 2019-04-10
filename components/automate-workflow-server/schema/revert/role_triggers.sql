-- Revert role_triggers

BEGIN;

DROP TRIGGER IF EXISTS ensure_membership
  ON enterprise_user_roles;

DROP TRIGGER IF EXISTS ensure_membership
  ON organization_user_roles;

COMMIT;
