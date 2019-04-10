-- Deploy delivery:projects_add_type to pg
-- requires: projects

BEGIN;

ALTER TABLE projects ADD COLUMN TYPE cd_project_type DEFAULT 'local';

COMMIT;
