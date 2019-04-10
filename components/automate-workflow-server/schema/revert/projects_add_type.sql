-- Revert delivery:projects_add_type from pg

BEGIN;

ALTER TABLE projects DROP COLUMN type;

COMMIT;
