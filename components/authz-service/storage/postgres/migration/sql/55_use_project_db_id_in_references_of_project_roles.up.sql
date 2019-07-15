BEGIN;
-- break all references
ALTER TABLE iam_role_projects
    DROP CONSTRAINT iam_role_projects_project_id_fkey;
ALTER TABLE iam_role_projects RENAME COLUMN project_id TO project_temp_id;
ALTER TABLE iam_role_projects
    ADD COLUMN project_id INTEGER REFERENCES iam_projects (db_id) ON DELETE CASCADE DEFERRABLE;
UPDATE
    iam_role_projects t
SET
    project_id = (
        SELECT
            db_id
        FROM
            iam_projects
        WHERE
            id = t.project_temp_id);
ALTER TABLE iam_role_projects
    DROP COLUMN project_temp_id;
-- returns NULL if there's no associated projects; the call can decide
-- (by coalesce'ing) if they want that to be {} or {(unassigned)}.

CREATE OR REPLACE FUNCTION role_projects (_role_id iam_roles.id % TYPE, OUT _project_ids TEXT[])
    RETURNS TEXT[]
    AS $$
DECLARE
    role_db_id INTEGER;
BEGIN
    SELECT
        db_id INTO STRICT role_db_id
    FROM
        iam_roles AS r
    WHERE
        r.id = _role_id;
    SELECT
        array_agg(p.id) INTO _project_ids
    FROM
        iam_role_projects AS rp
        JOIN iam_projects AS p ON rp.project_id = p.db_id
    WHERE
        rp.role_id = role_db_id;
END;
$$
LANGUAGE plpgsql;
CREATE OR REPLACE FUNCTION query_role (_role_id iam_roles.id % TYPE)
    RETURNS json
    AS $$
    SELECT
        json_build_object('id', r.id, 'name', r.name, 'type', r.type, 'actions', r.actions, 'projects', COALESCE(role_projects (r.id), '{}')) AS ROLE
    FROM
        iam_roles AS r
    WHERE
        r.id = _role_id
$$
LANGUAGE sql;
CREATE OR REPLACE FUNCTION query_roles (_projects_filter TEXT[])
    RETURNS SETOF json
    AS $$
    SELECT
        json_build_object('id', r.id, 'name', r.name, 'type', r.type, 'actions', r.actions, 'projects', COALESCE(role_projects (r.id), '{}')) AS ROLE
    FROM
        iam_roles AS r
    WHERE
        projects_match (COALESCE(role_projects (r.id), '{(unassigned)}'), _projects_filter);
$$
LANGUAGE sql;
-- Helper functions, now made strict: if they don't find anything,
-- a "not found" exception is thrown; if they find more than one thing,
-- they'll also throw an exception. (The latter should never happen.)

CREATE OR REPLACE FUNCTION project_db_id (_id iam_projects.id % TYPE, OUT _db_id iam_projects.db_id % TYPE)
    RETURNS iam_projects.db_id % TYPE
    AS $$
BEGIN
    SELECT
        db_id INTO STRICT _db_id
    FROM
        iam_projects
    WHERE
        id = _id;
END;
$$
LANGUAGE plpgsql;
CREATE OR REPLACE FUNCTION project_id (_db_id iam_projects.db_id % TYPE, OUT _id iam_projects.id % TYPE)
    RETURNS iam_projects.id % TYPE
    AS $$
BEGIN
    SELECT
        id INTO STRICT _id
    FROM
        iam_projects
    WHERE
        db_id = _db_id;
END;
$$
LANGUAGE plpgsql;
COMMIT;
