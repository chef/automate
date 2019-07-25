BEGIN;

-- lookup by name, the UUID column can go away
-- Note: no need to raise a custom exception here, either, as we're only
-- calling this function internally after having upsert'ed the members.
CREATE OR REPLACE FUNCTION member_db_id (
    _name iam_members.name % TYPE,
    OUT _db_id iam_members.db_id % TYPE
)
    RETURNS iam_members.db_id % TYPE
    AS $$
BEGIN
    SELECT
        db_id INTO STRICT _db_id
    FROM
        iam_members
    WHERE
        name = _name;
END;
$$
LANGUAGE plpgsql;

-- Note: we don't raise any customized exceptions here, since this is
-- never called using user-provided inputs. It's only called internally
-- when linking the statements to their policies; and when deleting a
-- policy's members.
CREATE OR REPLACE FUNCTION policy_db_id (
    _id iam_policies.id % TYPE,
    OUT _db_id iam_policies.db_id % TYPE
)
    RETURNS iam_policies.db_id % TYPE
    AS $$
BEGIN
    SELECT
        db_id INTO STRICT _db_id
    FROM
        iam_policies
    WHERE
        id = _id;
END;
$$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION role_db_id (
    _id iam_roles.id % TYPE,
    OUT _db_id iam_roles.db_id % TYPE
)
    RETURNS iam_roles.db_id % TYPE
    AS $$
BEGIN
    SELECT
        db_id INTO _db_id
    FROM
        iam_roles
    WHERE
        id = _id;
    IF NOT FOUND THEN
        RAISE foreign_key_violation
        USING MESSAGE = 'role not found: ' || _id;
    END IF;
END;
$$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION project_db_id (
    _id iam_projects.id % TYPE,
    OUT _db_id iam_projects.db_id % TYPE
)
    RETURNS iam_projects.db_id % TYPE
    AS $$
BEGIN
    SELECT
        db_id INTO _db_id
    FROM
        iam_projects
    WHERE
        id = _id;
    IF NOT FOUND THEN
        RAISE foreign_key_violation
        USING MESSAGE = 'project not found: ' || _id;
    END IF;
END;
$$
LANGUAGE plpgsql;

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
                ELSE
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
