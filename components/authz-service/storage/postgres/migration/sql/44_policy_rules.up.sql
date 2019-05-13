BEGIN;

CREATE TYPE iam_rule_type AS ENUM ('node', 'event');
CREATE TYPE iam_attributes AS ENUM ('organization', 'chef-server',
  'environment', 'role', 'chef-tag', 'policy-name', 'policy-group');
CREATE TYPE iam_condition_operator AS ENUM ('equals', 'member-of');

CREATE TABLE iam_project_rules (
  db_id SERIAL PRIMARY KEY,
  id TEXT NOT NULL UNIQUE,
  project_id TEXT REFERENCES iam_projects ON DELETE CASCADE,
  name TEXT NOT NULL,
  type iam_rule_type NOT NULL
);

CREATE TABLE iam_rule_conditions (
  db_id SERIAL PRIMARY KEY,
  rule_id INTEGER REFERENCES iam_project_rules ON DELETE CASCADE,
  value TEXT NOT NULL,
  attribute iam_attributes NOT NULL,
  operator iam_condition_operator NOT NULL
);

COMMIT;
