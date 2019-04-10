CREATE TABLE teams (
  id uuid PRIMARY KEY,
  name TEXT NOT NULL UNIQUE,
  description TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL
);
CREATE TABLE teams_users_associations (
  team_id uuid NOT NULL REFERENCES teams ON DELETE CASCADE,
  user_id uuid NOT NULL,
  PRIMARY KEY (team_id, user_id),
  created_at TIMESTAMPTZ NOT NULL
);