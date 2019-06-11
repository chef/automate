BEGIN;

CREATE TYPE staged_rule_state
  AS ENUM (
    'new',
    'deleted',
    'updated'
    );

CREATE TABLE iam_staged_project_rules (
  db_id SERIAL PRIMARY KEY,
  id TEXT NOT NULL UNIQUE,
  project_id TEXT REFERENCES iam_projects,
  name TEXT NOT NULL,
  type TEXT NOT NULL,
  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  state (pending)
);

CREATE TABLE iam_staged_rule_conditions (
  db_id SERIAL PRIMARY KEY,
  rule_db_id INTEGER REFERENCES iam_staged_project_rules ON DELETE CASCADE,
  value TEXT[] NOT NULL,
  attribute TEXT NOT NULL,
  operator TEXT NOT NULL,
  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  state staged_rule_state NOT NULL
);

COMMIT;