-- Deploy delivery:team_members to pg

BEGIN;

  CREATE TABLE IF NOT EXISTS team_members (
    team_id BIGINT REFERENCES teams(id) ON DELETE CASCADE,
    user_id BIGINT REFERENCES users(id) ON DELETE CASCADE,
    CONSTRAINT teams_team_id_user_id UNIQUE (team_id, user_id)
  );

COMMIT;
