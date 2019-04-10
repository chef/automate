-- Deploy delivery:projects_full_scm_module to pg
-- requires: projects_add_type

BEGIN;

ALTER TABLE projects ALTER COLUMN type TYPE TEXT;
ALTER TABLE projects RENAME COLUMN type TO scm_module;
ALTER TABLE projects ALTER COLUMN scm_module SET DEFAULT 'deliv_scm_local';
UPDATE projects SET scm_module='deliv_scm_local' WHERE scm_module='local';
UPDATE projects SET scm_module='deliv_scm_github' WHERE scm_module='github';
DROP TYPE cd_project_type;

COMMIT;
