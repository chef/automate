BEGIN;

CREATE TABLE iam_projects (
  db_id SERIAL,
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  type iam_policy_type NOT NULL DEFAULT 'custom'
);

CREATE OR REPLACE FUNCTION
  query_project(_project_id TEXT)
  RETURNS json AS $$

  WITH t AS
    (SELECT p.id, p.name, p.type FROM iam_projects p
      WHERE p.id = _project_id)
  SELECT row_to_json(t) AS role FROM t;

$$ LANGUAGE sql;

COMMIT;
