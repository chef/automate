-- Revert project_user_roles

BEGIN;

DROP TABLE IF EXISTS project_user_roles;

COMMIT;
