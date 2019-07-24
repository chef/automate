BEGIN;

-- lookup by name, the UUID column can go away
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

COMMIT;

