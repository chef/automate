-- under IAM v1, teams were identified by auto-generated UUIDs
-- under IAM v2, unique IDs are provided by the user
ALTER TABLE teams DROP COLUMN id;

-- under IAM v1, team names were unique identifiers
-- under IAM v2, IDs are the unique human-readable identifiers, 
-- though it is the db_id that is used as the identifier among db tables
ALTER TABLE teams RENAME COLUMN name TO id;

--- rename uniqueness constraint
ALTER TABLE teams DROP CONSTRAINT teams_name_key;
ALTER TABLE teams ADD CONSTRAINT teams_id_key UNIQUE (id);

-- under IAM v1, team descriptions were human-readable names
-- under IAM v2, the name field serves this purpose
ALTER TABLE teams RENAME COLUMN description TO name;

-- add helper for team db_id lookup
CREATE OR REPLACE FUNCTION team_db_id (_id text)
    RETURNS INTEGER
    AS $$
    SELECT
        db_id
    FROM
        teams
    WHERE
        id = _id;
$$
LANGUAGE SQL;
