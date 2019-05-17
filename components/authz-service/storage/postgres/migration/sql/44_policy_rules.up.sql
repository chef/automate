BEGIN;

CREATE TABLE iam_project_rules (
  db_id SERIAL PRIMARY KEY,
  id TEXT NOT NULL UNIQUE,
  project_id TEXT REFERENCES iam_projects ON DELETE CASCADE,
  name TEXT NOT NULL,
  type TEXT NOT NULL
);

CREATE TABLE iam_rule_conditions (
  db_id SERIAL PRIMARY KEY,
  rule_db_id INTEGER REFERENCES iam_project_rules ON DELETE CASCADE,
  value TEXT[] NOT NULL,
  attribute TEXT NOT NULL,
  operator TEXT NOT NULL
);

CREATE OR REPLACE FUNCTION
  projects_match_for_rule(_project_id TEXT, _projects_filter TEXT[])
  RETURNS BOOLEAN AS $$
    BEGIN
      RETURN (
        array_length(_projects_filter, 1) IS NULL
        OR _project_id = ANY (_projects_filter)
      );
    END
$$ LANGUAGE PLPGSQL;

CREATE OR REPLACE FUNCTION
  query_rule(_rule_db_id TEXT, _project_filter TEXT[])
  RETURNS json AS $$

  WITH t AS (
    SELECT
      r.id,
      r.project_id,
      r.name,
      r.type,
      -- A rule can't exist without conditions so we don't need to worry
      -- about null case here.
      json_agg(rc) AS conditions
    FROM iam_project_rules AS r
    LEFT OUTER JOIN iam_rule_conditions
    AS rc ON rc.rule_db_id=r.db_id
    WHERE id=_rule_db_id AND projects_match_for_rule(project_id, _project_filter)
    GROUP BY r.id, r.project_id, r.name, r.type
  )
  SELECT row_to_json(t) AS rule FROM t;

$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION
  query_rules(_project_filter TEXT[])
  RETURNS setof json AS $$

  WITH t AS (
    SELECT
      r.id,
      r.project_id,
      r.name,
      r.type,
      -- A rule can't exist without conditions so we don't need to worry
      -- about null case here.
      json_agg(rc) AS conditions
    FROM iam_project_rules AS r
    LEFT OUTER JOIN iam_rule_conditions AS rc
    ON rc.rule_db_id=r.db_id
    WHERE projects_match_for_rule(project_id, _project_filter)
    GROUP BY r.id, r.project_id, r.name, r.type
  )
  SELECT row_to_json(t) AS rule FROM t;

$$ LANGUAGE sql;

COMMIT;
