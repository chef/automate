BEGIN;
ALTER TABLE iam_policy_projects
    DROP CONSTRAINT iam_policy_projects_project_id_fkey;
ALTER TABLE iam_policy_projects RENAME COLUMN project_id TO project_temp_id;
ALTER TABLE iam_policy_projects ADD COLUMN project_id SERIAL;

UPDATE iam_policy_projects t
SET
    project_id = (
        SELECT
            db_id
        FROM
            iam_projects
        WHERE
            id = t.project_temp_id);
ALTER TABLE iam_policy_projects
    DROP COLUMN project_temp_id;

ALTER TABLE iam_policy_projects ADD CONSTRAINT "iam_policy_projects_policy_id_project_id_unique" UNIQUE (policy_id, project_id);

CREATE OR REPLACE FUNCTION policy_projects(_policy_id iam_policies.id%TYPE, OUT _project_ids TEXT[])
    RETURNS TEXT[]
    AS $$
DECLARE
    policy_db_id INTEGER;
BEGIN
    SELECT
        db_id INTO STRICT policy_db_id
    FROM
        iam_policies AS pol
    WHERE
        pol.id = _policy_id;
    SELECT
        array_agg(p.id) INTO _project_ids
    FROM
        iam_policy_projects AS pp
        JOIN iam_projects AS p ON pp.project_id = p.db_id
    WHERE
        pp.policy_id = policy_db_id;
END;
$$ LANGUAGE plpgsql;

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
            LEFT OUTER JOIN iam_projects AS proj ON pol_projs.project_id = proj.db_id WHERE pol_projs.policy_id = pol.db_id) AS projects FROM iam_policies AS pol WHERE pol.id = _policy_id
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
        LEFT OUTER JOIN iam_projects AS proj ON pol_projs.project_id = proj.db_id WHERE pol_projs.policy_id = pol.db_id) AS projects FROM iam_policies AS pol
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
COMMIT;
