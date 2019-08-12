ALTER TABLE teams ADD COLUMN deletable BOOLEAN NOT NULL DEFAULT TRUE;

INSERT INTO teams
    VALUES (uuid_generate_v4(), 'admins',
            'Members of the admins team, by default, have access to all parts of the API.',
            CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, FALSE)
    ON CONFLICT (name) DO UPDATE SET deletable=FALSE;

CREATE OR REPLACE FUNCTION cannot_delete_team_error() RETURNS trigger AS $$
BEGIN
    RAISE EXCEPTION 'You cannot delete % with id % as it is not marked as deletable.', OLD.name, OLD.id USING
        ERRCODE='DRPTM';
END$$ LANGUAGE plpgsql;

CREATE TRIGGER only_allow_delete_on_deletable_teams
    BEFORE DELETE ON TEAMS
    FOR EACH ROW
    WHEN (OLD.deletable = FALSE)
    EXECUTE PROCEDURE cannot_delete_team_error();
