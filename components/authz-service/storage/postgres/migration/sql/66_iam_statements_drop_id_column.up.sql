BEGIN;

ALTER TABLE iam_statements DROP COLUMN id;

-- we change the types, so we'll first remove the old one
DROP FUNCTION insert_iam_statement_into_policy(TEXT, UUID, iam_effect, TEXT[], TEXT[], TEXT, TEXT[]);
DROP FUNCTION statement_db_id(UUID);

CREATE OR REPLACE FUNCTION
    insert_iam_statement_into_policy(_policy_id TEXT, _statement_effect iam_effect, _statement_actions TEXT[],
    _statement_resources TEXT[], _statement_role TEXT, _statement_projects TEXT[])
RETURNS void AS $$
DECLARE
  statement_db_id INTEGER;
BEGIN
    -- if NULL or an empty string was passed for the role, we shouldn't try to insert a role.
    IF _statement_role IS NULL OR _statement_role=''
    THEN
        INSERT INTO iam_statements (policy_id, effect, actions, resources)
        VALUES (policy_db_id(_policy_id), _statement_effect, _statement_actions, _statement_resources)
        RETURNING db_id INTO statement_db_id;
    ELSE
        INSERT INTO iam_statements (policy_id, effect, actions, resources, role_id)
        VALUES (policy_db_id(_policy_id), _statement_effect, _statement_actions, _statement_resources, role_db_id(_statement_role))
        RETURNING db_id INTO statement_db_id;
    END IF;
    INSERT INTO iam_statement_projects (statement_id, project_id)
      SELECT statement_db_id, project_db_id(p_id)
      FROM UNNEST(_statement_projects) projects(p_id)
    ON CONFLICT DO NOTHING;
    RETURN;
END;
$$
LANGUAGE plpgsql;

-- removed stmt.id
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
                        stmt.db_id,
                        stmt.effect,
                        stmt.actions,
                        stmt.resources,
                        (
                          SELECT COALESCE((SELECT id FROM iam_roles WHERE db_id=stmt.role_id), '') AS role
                        ),
                        (
                            SELECT
                                COALESCE(json_agg(proj.id) FILTER (WHERE proj.id IS NOT NULL),
                                    '[]')
                                FROM iam_statement_projects AS stmt_projs
                            LEFT JOIN iam_projects AS proj
                            ON stmt_projs.statement_id = stmt.db_id
                            WHERE stmt_projs.project_id = proj.db_id
                        ) AS projects
                    FROM iam_statements stmt
                    JOIN iam_policies ON stmt.policy_id = pol.db_id
                GROUP BY
                    stmt.db_id,
                    stmt.effect,
                    stmt.actions,
                    stmt.resources,
                    stmt.role_id)
            SELECT
                array_agg(statement_rows)
            FROM statement_rows) AS statements,
            -- get policy members
            (
            SELECT
                array_agg(mem)
            FROM iam_policy_members AS pol_mems
            LEFT JOIN iam_members AS mem ON pol_mems.member_id = mem.db_id WHERE pol_mems.policy_id = pol.db_id) AS members,
            -- get projects
            (
            SELECT
                array_agg(proj.id) FILTER (WHERE proj.id IS NOT NULL)
            FROM iam_policy_projects AS pol_projs
            LEFT JOIN iam_projects AS proj
            ON pol_projs.project_id = proj.db_id
            WHERE pol_projs.policy_id = pol.db_id
        ) AS projects
    FROM iam_policies AS pol
    WHERE pol.id = _policy_id
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

-- removed stmt.id
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
                        stmt.db_id,
                        stmt.effect,
                        stmt.actions,
                        stmt.resources,
                        (
                          SELECT COALESCE((SELECT id FROM iam_roles WHERE db_id=stmt.role_id), '') AS role
                        ),
                        (
                            SELECT
                                COALESCE(json_agg(proj.id) FILTER (WHERE proj.id IS NOT NULL),
                                    '[]')
                                FROM iam_statement_projects AS stmt_projs
                            LEFT JOIN iam_projects AS proj ON stmt_projs.statement_id = stmt.db_id WHERE stmt_projs.project_id = proj.db_id) AS projects FROM iam_statements stmt
                    JOIN iam_policies ON stmt.policy_id = pol.db_id
                GROUP BY
                    stmt.db_id,
                    stmt.effect,
                    stmt.actions,
                    stmt.resources,
                    stmt.role_id
)
            SELECT array_agg(statement_rows) FROM statement_rows) AS statements,
            ( SELECT
                array_agg(mem)
              FROM iam_policy_members AS pol_mems
              LEFT JOIN iam_members AS mem ON pol_mems.member_id = mem.db_id WHERE pol_mems.policy_id = pol.db_id
            ) AS members,
    ( SELECT
        array_agg(proj.id) FILTER (WHERE proj.id IS NOT NULL)
        FROM iam_policy_projects AS pol_projs
        LEFT JOIN iam_projects AS proj ON pol_projs.project_id = proj.db_id WHERE pol_projs.policy_id = pol.db_id
    ) AS projects FROM iam_policies AS pol
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
