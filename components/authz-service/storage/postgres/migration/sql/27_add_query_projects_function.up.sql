-- only one statement, no need for a transaction
CREATE OR REPLACE FUNCTION
  query_projects()
  RETURNS setof json AS $$

  WITH t AS
    (SELECT p.id, p.name, p.type FROM iam_projects p)
  SELECT row_to_json(t) AS project FROM t;

$$ LANGUAGE sql;
