-- Deploy delivery:cd_project_type to pg

BEGIN;

CREATE TYPE cd_project_type AS ENUM('local',
                                    'github');

COMMENT ON TYPE cd_project_type IS
$$
When appended to `deliv_scm_` should name an Erlang module in server.
$$;

COMMIT;
