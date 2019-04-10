BEGIN;

CREATE OR REPLACE FUNCTION
  query_projects()
  RETURNS setof json AS $$

  WITH t AS
    (SELECT p.id, p.name, p.type, p.projects FROM iam_projects p)
  SELECT row_to_json(t) AS project FROM t;

$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION
  query_project(_project_id TEXT)
  RETURNS json AS $$

  WITH t AS
    (SELECT p.id, p.name, p.type, p.projects FROM iam_projects p
      WHERE p.id = _project_id)
  SELECT row_to_json(t) AS role FROM t;

$$ LANGUAGE sql;

COMMIT;
