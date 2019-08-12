BEGIN;
-- break reference constraints
ALTER TABLE teams_users_associations
    ADD COLUMN team_db_id SERIAL;
ALTER TABLE teams_users_associations
    DROP CONSTRAINT teams_users_associations_pkey;
ALTER TABLE teams_users_associations
    DROP CONSTRAINT teams_users_associations_team_id_fkey;
-- massage teams for new pkey
ALTER TABLE teams
    DROP CONSTRAINT teams_pkey;
ALTER TABLE teams
    ADD CONSTRAINT teams_id_unique UNIQUE (id);
ALTER TABLE teams
    ADD COLUMN db_id SERIAL PRIMARY KEY;
-- set the team_db_id fields in the assoc table
UPDATE
    teams_users_associations
SET
    team_db_id = db_id
FROM
    teams
WHERE
    teams.id = team_id;
ALTER TABLE teams_users_associations
    DROP COLUMN team_id;
-- add new constraints
ALTER TABLE teams_users_associations
    ADD CONSTRAINT teams_db_id_fkey FOREIGN KEY (team_db_id) REFERENCES teams (db_id) ON DELETE CASCADE;
ALTER TABLE teams_users_associations
    ADD CONSTRAINT teams_users_pkey PRIMARY KEY (team_db_id, user_id);
-- add helper for team db_id lookup
CREATE FUNCTION team_db_id (_id UUID)
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
COMMIT;
