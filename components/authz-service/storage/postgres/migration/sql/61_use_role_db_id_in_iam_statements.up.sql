BEGIN;

-- there was previously a bug that allowed NULL values to be inserted into
-- the database for actions (and maybe resources). clean up any such instances.
UPDATE iam_statements t SET actions = '{}' WHERE actions IS NULL;
ALTER TABLE iam_statements ALTER COLUMN actions SET NOT NULL;
UPDATE iam_statements t SET resources = '{}' WHERE resources IS NULL;
ALTER TABLE iam_statements ALTER COLUMN resources SET NOT NULL;

-- also set policy_id to NOT NULL since a statement can't exist
-- outside of a policy. there should not be any instances of
-- orphaned statements, but adding the delete line just in case
-- so migrations don't explode if there are any somehow (they
-- are useless if they exist anyway).
DELETE FROM iam_statements WHERE policy_id IS NULL;
ALTER TABLE iam_statements ALTER COLUMN policy_id SET NOT NULL;

ALTER TABLE iam_statements ADD COLUMN role_id INTEGER;

-- populate role_id in all statements that have a role
UPDATE iam_statements t
  SET role_id = (SELECT db_id FROM iam_roles WHERE id = t.role)
  WHERE role != '';

ALTER TABLE iam_statements DROP COLUMN role;

ALTER TABLE iam_statements
    ADD CONSTRAINT iam_statements_role_id_fkey FOREIGN KEY (role_id) REFERENCES iam_roles(db_id);

-- This needs to be DEFERRABLE INITIALLY DEFERRED so that we don't get a FKEY error before
-- the trigger works it's magic below.
ALTER TABLE iam_statements ALTER CONSTRAINT iam_statements_role_id_fkey DEFERRABLE INITIALLY DEFERRED;

-- i wanted to use the above FKEY constraint with ON DELETE to clean up any statement where the
-- role getting removed would result in a NULL role_id in iam_statements. that is doable, but
-- what becomes tricky is adding the additional logic to completely remove the statement if the affected
-- policy would also have no actions. so instead of splitting that logic up, let's handle it all in a trigger.

CREATE OR REPLACE FUNCTION purge_statements_with_no_actions_or_role() RETURNS TRIGGER AS $$
  BEGIN
    -- for statements that will still have actions, simply remove the role since those
    -- statements are still valid
    UPDATE iam_statements SET role_id=NULL WHERE role_id=OLD.db_id AND actions != '{}';

    -- for statements that become invalid (no role or actions), delete them
    DELETE FROM iam_statements WHERE role_id=OLD.db_id AND actions = '{}';

    -- if as a result, a policy now has no statements, remove the policy unless it's
    -- chef-managed (chef-managed case should never happen since chef-managed roles can't be
    -- deleted, but just to be safe adding it in)
    DELETE FROM iam_policies USING iam_policies AS p
      LEFT OUTER JOIN iam_statements AS s ON p.db_id=s.policy_id
      WHERE s.policy_id IS NULL AND iam_policies.id=p.id AND p.type != 'chef-managed';

    RETURN NULL;
  END
$$
LANGUAGE plpgsql;

CREATE TRIGGER on_role_deletion AFTER DELETE ON iam_roles
FOR EACH ROW
EXECUTE PROCEDURE purge_statements_with_no_actions_or_role();

CREATE OR REPLACE FUNCTION role_db_id (_id TEXT)
    RETURNS INTEGER
    AS $$
    SELECT
        db_id
    FROM
        iam_roles
    WHERE
        id = _id;
$$
LANGUAGE SQL;

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
                        stmt.id,
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
                            LEFT OUTER JOIN iam_projects AS proj
                            ON stmt_projs.statement_id = stmt.db_id
                            WHERE stmt_projs.project_id = proj.db_id
                        ) AS projects 
                    FROM iam_statements stmt
                    INNER JOIN iam_policies ON stmt.policy_id = pol.db_id
                GROUP BY
                    stmt.db_id,
                    stmt.id,
                    stmt.effect,
                    stmt.actions,
                    stmt.resources,
                    stmt.role_id)
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
                        stmt.db_id,
                        stmt.id,
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
                            LEFT OUTER JOIN iam_projects AS proj ON stmt_projs.statement_id = stmt.db_id WHERE stmt_projs.project_id = proj.db_id) AS projects FROM iam_statements stmt
                    INNER JOIN iam_policies ON stmt.policy_id = pol.db_id
                GROUP BY
                    stmt.db_id,
                    stmt.id,
                    stmt.effect,
                    stmt.actions,
                    stmt.resources,
                    stmt.role_id
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

CREATE OR REPLACE FUNCTION
    insert_iam_statement_into_policy(_policy_id TEXT, _statement_id UUID, _statement_effect iam_effect, _statement_actions TEXT[],
    _statement_resources TEXT[], _statement_role TEXT, _statement_projects TEXT[])
        RETURNS void AS $$
            BEGIN
                -- if NULL or an empty string was passed for the role, we shouldn't try to insert a role.
                IF _statement_role IS NULL OR _statement_role=''
                THEN
                    INSERT INTO iam_statements (policy_id, id, effect, actions, resources)
                        VALUES (policy_db_id(_policy_id), _statement_id, _statement_effect, _statement_actions, _statement_resources);

                -- otherwise, we should try to insert a role. however, we want to catch the case where role_db_id returns NULL.
                -- if we don't then the insert will just insert NULL for role_id, which is not what we want if a role was passed in.
                ELSE
                    IF role_db_id(_statement_role) IS NULL
                    THEN
                        RAISE EXCEPTION 'no role exists with ID %', _statement_role USING ERRCODE = 'RDNES';
                    END IF;

                    INSERT INTO iam_statements (policy_id, id, effect, actions, resources, role_id)
                        VALUES (policy_db_id(_policy_id), _statement_id, _statement_effect, _statement_actions, _statement_resources, role_db_id(_statement_role));
                END IF;

                INSERT INTO iam_statement_projects (statement_id, project_id)
                    SELECT statement_db_id(_statement_id) s_id, project_db_id(p_id)
                    FROM UNNEST(_statement_projects) project_db_id(p_id)
                ON CONFLICT DO NOTHING;
                RETURN;
            END
$$ LANGUAGE PLPGSQL;

COMMIT;
