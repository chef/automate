BEGIN;
ALTER TABLE iam_policy_statements
    DROP CONSTRAINT iam_policy_statements_pkey,
    DROP CONSTRAINT iam_policy_statements_policy_id_fkey;
ALTER TABLE iam_statement_projects
    DROP CONSTRAINT iam_statement_projects_statement_id_fkey;
ALTER TABLE iam_policy_statements
    ADD COLUMN policy_db_id SERIAL;
UPDATE
    iam_policy_statements ips
SET
    policy_db_id = (
        SELECT
            db_id
        FROM
            iam_policies
        WHERE
            id = ips.policy_id);
ALTER TABLE iam_statements
    DROP CONSTRAINT iam_statements_pkey;
ALTER TABLE iam_statements
    ADD COLUMN db_id SERIAL PRIMARY KEY,
    ADD COLUMN policy_id INTEGER REFERENCES iam_policies(db_id) ON DELETE CASCADE DEFERRABLE,
    ADD UNIQUE (id);
UPDATE
    iam_statements iams
SET
    policy_id = (
        SELECT
            policy_db_id
        FROM
            iam_policy_statements
        WHERE
            statement_id = iams.id);
DROP TABLE iam_policy_statements CASCADE;
ALTER TABLE iam_statement_projects
    ADD UNIQUE (statement_id, project_id),
    ADD CONSTRAINT iam_statement_projects_statement_id_fkey FOREIGN KEY (statement_id) REFERENCES iam_statements(id) ON DELETE CASCADE DEFERRABLE;

-- add helper for statement db_id lookup
CREATE OR REPLACE FUNCTION statement_db_id (_id iam_statements.id % TYPE, OUT _db_id iam_statements.db_id % TYPE)
    RETURNS iam_statements.db_id % TYPE
    AS $$
BEGIN
    SELECT
        db_id INTO STRICT _db_id
    FROM
        iam_statements
    WHERE
        id = _id;
END;
$$
LANGUAGE plpgsql;
CREATE OR REPLACE FUNCTION query_policy (_policy_id TEXT, _projects_filter TEXT[])
    RETURNS json
    AS $$
    WITH temp AS (
        SELECT
            pol.db_id,
            pol.id,
            pol.name,
            pol.type,
            -- get policy's statements using temporary table
            ( WITH statement_rows AS (
                    SELECT
                        stmt.id,
                        stmt.effect,
                        stmt.actions,
                        stmt.resources,
                        stmt.role,
                        -- get each statement's projects by cross-referencing iam_policy_statements and iam_statement_projects
                        (
                            SELECT
                                COALESCE(json_agg(proj.id) FILTER (WHERE proj.id IS NOT NULL),
                                    '[]')
                                FROM iam_statement_projects AS stmt_projs
                            LEFT OUTER JOIN iam_projects AS proj ON stmt_projs.statement_id = stmt.id WHERE stmt_projs.project_id = proj.id) AS projects
                    FROM iam_statements stmt
                    INNER JOIN iam_policies
                    ON stmt.policy_id=pol.db_id
                GROUP BY
                    stmt.id, stmt.effect, stmt.actions, stmt.resources, stmt.role)
            SELECT
                array_agg(statement_rows) FILTER (WHERE statement_rows.id IS NOT NULL)
            FROM statement_rows) AS statements,
            -- get policy members
            (
            SELECT
                array_agg(mem) FILTER (WHERE mem.id IS NOT NULL)
            FROM iam_policy_members AS pol_mems
        LEFT OUTER JOIN iam_members AS mem ON pol_mems.member_id = mem.db_id WHERE pol_mems.policy_id = pol.db_id) AS members,
            -- get projects
            (
            SELECT
                array_agg(proj.id) FILTER (WHERE proj.id IS NOT NULL)
            FROM iam_policy_projects AS pol_projs
            LEFT OUTER JOIN iam_projects AS proj ON pol_projs.project_id = proj.id WHERE pol_projs.policy_id = pol.db_id) AS projects FROM iam_policies AS pol WHERE pol.id = _policy_id
        GROUP BY
            pol.db_id,
            pol.id,
            pol.name,
            pol.type
)
SELECT
    json_build_object('id', temp.id, 'name', temp.name, 'type', temp.type, 'statements', COALESCE(temp.statements, '{}'), 'members', COALESCE(temp.members, '{}'), 'projects', COALESCE(temp.projects, '{}')) AS POLICY
FROM
    temp
WHERE
    projects_match (temp.projects::TEXT[], _projects_filter);
$$
LANGUAGE sql;

CREATE OR REPLACE FUNCTION query_policies (_projects_filter TEXT[])
    RETURNS SETOF json
    AS $$
    WITH temp AS (
        SELECT
            pol.db_id,
            pol.id,
            pol.name,
            pol.type,
            ( WITH statement_rows AS (
                    SELECT
                        stmt.id,
                        stmt.effect,
                        stmt.actions,
                        stmt.resources,
                        stmt.role,
                        (
                            SELECT
                                COALESCE(json_agg(proj.id) FILTER (WHERE proj.id IS NOT NULL),
                                    '[]')
                                FROM iam_statement_projects AS stmt_projs
                            LEFT OUTER JOIN iam_projects AS proj ON stmt_projs.statement_id = stmt.id WHERE stmt_projs.project_id = proj.id) AS projects
                    FROM iam_statements stmt
                    INNER JOIN iam_policies
                    ON stmt.policy_id=pol.db_id
                GROUP BY
                    stmt.id, stmt.effect, stmt.actions, stmt.resources, stmt.role
)
            SELECT
                array_agg(statement_rows) FILTER (WHERE statement_rows.id IS NOT NULL)
                FROM statement_rows) AS statements,
            (
            SELECT
                array_agg(mem) FILTER (WHERE mem.id IS NOT NULL)
                FROM iam_policy_members AS pol_mems
            LEFT OUTER JOIN iam_members AS mem ON pol_mems.member_id = mem.db_id WHERE pol_mems.policy_id = pol.db_id) AS members,
    (
    SELECT
        array_agg(proj.id) FILTER (WHERE proj.id IS NOT NULL)
        FROM iam_policy_projects AS pol_projs
        LEFT OUTER JOIN iam_projects AS proj ON pol_projs.project_id = proj.id WHERE pol_projs.policy_id = pol.db_id) AS projects FROM iam_policies AS pol
GROUP BY
    pol.db_id,
    pol.id,
    pol.name,
    pol.type
)
SELECT
    json_build_object('id', temp.id, 'name', temp.name, 'type', temp.type, 'statements', COALESCE(temp.statements, '{}'), 'members', COALESCE(temp.members, '{}'), 'projects', COALESCE(temp.projects, '{}')) AS POLICY
FROM
    temp
WHERE
    projects_match (temp.projects::TEXT[], _projects_filter);
$$
LANGUAGE sql;

CREATE OR REPLACE FUNCTION
    insert_iam_statement_into_policy(_policy_id TEXT, _statement_id UUID, _statement_effect iam_effect, _statement_actions TEXT[],
    _statement_resources TEXT[], _statement_role TEXT, _statement_projects TEXT[])
        RETURNS void AS $$
            INSERT INTO iam_statements (policy_id, id, effect, actions, resources, role)
            VALUES (policy_db_id(_policy_id), _statement_id, _statement_effect, _statement_actions, _statement_resources, _statement_role);
                INSERT INTO iam_statement_projects (statement_id, project_id) 
                SELECT _statement_id s_id, p_id
                FROM UNNEST(_statement_projects) p_id
            ON CONFLICT DO NOTHING
$$
LANGUAGE sql;
COMMIT; 
