BEGIN;

CREATE OR REPLACE FUNCTION
  query_projects(_projects_filter TEXT[])
  RETURNS setof json AS $$

  WITH t AS
    (SELECT p.id, p.name, p.type, p.projects FROM iam_projects p
        WHERE array_length(_projects_filter, 1) IS NULL OR p.projects && _projects_filter)
  SELECT row_to_json(t) AS project FROM t;

$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION
  query_project(_project_id TEXT, _projects_filter TEXT[])
  RETURNS json AS $$

  WITH t AS
    (SELECT p.id, p.name, p.type, p.projects FROM iam_projects p
      WHERE p.id = _project_id AND (array_length(_projects_filter, 1) IS NULL OR p.projects && _projects_filter))
  SELECT row_to_json(t) AS role FROM t;

$$ LANGUAGE sql;

COMMIT;
