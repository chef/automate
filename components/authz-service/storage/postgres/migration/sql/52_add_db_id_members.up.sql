BEGIN;
ALTER TABLE iam_policy_members
    DROP CONSTRAINT iam_policy_members_member_id_fkey;
ALTER TABLE iam_members
    DROP CONSTRAINT iam_members_pkey;
ALTER TABLE iam_members
    ADD COLUMN db_id SERIAL PRIMARY KEY UNIQUE;
ALTER TABLE iam_policy_members RENAME COLUMN member_id TO member_temp_id;
ALTER TABLE iam_policy_members
    ADD COLUMN member_id INTEGER REFERENCES iam_members (db_id) ON DELETE CASCADE;
UPDATE
    iam_policy_members ipm
SET
    member_id = (
        SELECT
            db_id
        FROM
            iam_members
        WHERE
            id = ipm.member_temp_id);
ALTER TABLE iam_policy_members
    DROP COLUMN member_temp_id;
ALTER TABLE iam_members
    ADD UNIQUE (id, name);
ALTER TABLE iam_policy_members
    ADD UNIQUE (member_id, policy_id);
-- add helper for member db_id lookup
CREATE FUNCTION member_db_id (_id UUID)
    RETURNS INTEGER
    AS $$
    SELECT
        db_id
    FROM
        iam_members
    WHERE
        id = _id;
$$
LANGUAGE SQL;

CREATE OR REPLACE FUNCTION query_policy (_policy_id TEXT, _projects_filter TEXT[])
    RETURNS json
    AS $$
    WITH temp AS (
        SELECT
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
                                COALESCE(Json_agg(proj.id) FILTER (WHERE proj.id IS NOT NULL),
                                    '[]')
                                FROM iam_statement_projects AS stmt_projs
                            LEFT OUTER JOIN iam_projects AS proj ON stmt_projs.statement_id = stmt.id WHERE stmt_projs.project_id = proj.id) AS projects FROM iam_policy_statements AS pol_stmts
                    LEFT OUTER JOIN iam_statements AS stmt ON pol_stmts.statement_id = stmt.id WHERE pol_stmts.policy_id = pol.id
                GROUP BY
                    stmt.id)
            SELECT
                array_agg(statement_rows) FILTER (WHERE statement_rows.id IS NOT NULL)
            FROM statement_rows) AS statements,
            -- get policy members
            (
            SELECT
                array_agg(mem) FILTER (WHERE mem.id IS NOT NULL)
            FROM iam_policy_members AS pol_mems
        LEFT OUTER JOIN iam_members AS mem ON pol_mems.member_id = mem.db_id WHERE pol_mems.policy_id = pol.id) AS members,
            -- get projects
            (
            SELECT
                array_agg(proj.id) FILTER (WHERE proj.id IS NOT NULL)
            FROM iam_policy_projects AS pol_projs
            LEFT OUTER JOIN iam_projects AS proj ON pol_projs.project_id = proj.id WHERE pol_projs.policy_id = pol.id) AS projects FROM iam_policies AS pol WHERE pol.id = _policy_id
        GROUP BY
            pol.id
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
                            LEFT OUTER JOIN iam_projects AS proj ON stmt_projs.statement_id = stmt.id WHERE stmt_projs.project_id = proj.id) AS projects FROM iam_policy_statements AS pol_stmts
                    LEFT OUTER JOIN iam_statements AS stmt ON pol_stmts.statement_id = stmt.id WHERE pol_stmts.policy_id = pol.id
                GROUP BY
                    stmt.id
)
            SELECT
                array_agg(statement_rows) FILTER (WHERE statement_rows.id IS NOT NULL)
                FROM statement_rows) AS statements,
            (
            SELECT
                array_agg(mem) FILTER (WHERE mem.id IS NOT NULL)
                FROM iam_policy_members AS pol_mems
            LEFT OUTER JOIN iam_members AS mem ON pol_mems.member_id = mem.db_id WHERE pol_mems.policy_id = pol.id) AS members,
    (
    SELECT
        array_agg(proj.id) FILTER (WHERE proj.id IS NOT NULL)
        FROM iam_policy_projects AS pol_projs
        LEFT OUTER JOIN iam_projects AS proj ON pol_projs.project_id = proj.id WHERE pol_projs.policy_id = pol.id) AS projects FROM iam_policies AS pol
GROUP BY
    pol.id
)
SELECT
    json_build_object('id', temp.id, 'name', temp.name, 'type', temp.type, 'statements', COALESCE(temp.statements, '{}'), 'members', COALESCE(temp.members, '{}'), 'projects', COALESCE(temp.projects, '{}')) AS POLICY
FROM
    temp
WHERE
    projects_match (temp.projects::TEXT[], _projects_filter);
$$
LANGUAGE sql;
COMMIT;

