-- Revert delivery:cd_project_type from pg

BEGIN;

DROP TYPE IF EXISTS cd_project_type;

COMMIT;
