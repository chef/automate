-- Revert organization_user_roles

BEGIN;

DROP TABLE IF EXISTS organization_user_roles;

COMMIT;
