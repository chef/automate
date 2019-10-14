BEGIN;

CREATE TABLE iam_projects_graveyard (
  db_id SERIAL PRIMARY KEY,
  id TEXT NOT NULL UNIQUE
);

COMMIT;
