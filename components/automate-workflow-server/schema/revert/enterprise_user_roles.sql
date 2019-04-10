-- Revert enterprise_user_roles

BEGIN;

DROP TABLE IF EXISTS enterprise_user_roles;

COMMIT;
