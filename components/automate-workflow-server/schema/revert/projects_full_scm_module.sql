-- Revert delivery:projects_full_scm_module from pg

BEGIN;

CREATE TYPE cd_project_type AS ENUM('local',
                                    'github');

COMMENT ON TYPE cd_project_type IS
$$
When appended to `deliv_scm_` should name an Erlang module in server.
$$;

ALTER TABLE projects RENAME COLUMN scm_module TO type;
ALTER TABLE projects ALTER COLUMN type DROP DEFAULT;
UPDATE projects SET type='local' WHERE type='deliv_scm_local';
UPDATE projects SET type='github' WHERE type='deliv_scm_github';
ALTER TABLE projects ALTER COLUMN type TYPE cd_project_type USING type::cd_project_type;
ALTER TABLE projects ALTER COLUMN type SET DEFAULT 'local'::cd_project_type;

COMMIT;
