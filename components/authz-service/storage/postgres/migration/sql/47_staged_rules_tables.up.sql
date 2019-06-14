BEGIN;

CREATE TABLE iam_staged_project_rules (
  db_id SERIAL PRIMARY KEY,
  id TEXT NOT NULL UNIQUE,
  project_id TEXT REFERENCES iam_projects ON DELETE CASCADE,
  name TEXT NOT NULL,
  type TEXT NOT NULL,
  deleted BOOLEAN NOT NULL DEFAULT FALSE
);

CREATE TABLE iam_staged_rule_conditions (
  db_id SERIAL PRIMARY KEY,
  rule_db_id INTEGER REFERENCES iam_staged_project_rules ON DELETE CASCADE,
  value TEXT[] NOT NULL,
  attribute TEXT NOT NULL,
  operator TEXT NOT NULL,
  deleted BOOLEAN NOT NULL DEFAULT FALSE
);

CREATE OR REPLACE FUNCTION
  query_staged_rule(_rule_id TEXT, _project_filter TEXT[])
  RETURNS json AS $$

  WITH t AS (
    -- check for staged rule
    SELECT
      r.id,
      r.project_id,
      r.name,
      r.type,
      r.deleted,
      -- A rule can't exist without conditions so we don't need to worry
      -- about null case here.
      json_agg(rc) AS conditions
    FROM iam_staged_project_rules AS r
    LEFT OUTER JOIN iam_staged_rule_conditions
    AS rc ON rc.rule_db_id=r.db_id
    WHERE id=_rule_id AND projects_match_for_rule(project_id, _project_filter)
    GROUP BY r.id, r.project_id, r.name, r.type, r.deleted

    UNION ALL

    -- check for applied rule
    SELECT 
      r.id,
      r.project_id,
      r.name,
      r.type,
      false as deleted,
      json_agg(rc) AS conditions
      FROM iam_project_rules AS r
      LEFT OUTER JOIN iam_rule_conditions
      AS rc ON rc.rule_db_id=r.db_id
      WHERE id=_rule_id AND projects_match_for_rule(project_id, _project_filter)
      AND
        NOT EXISTS (SELECT 1 FROM iam_staged_project_rules WHERE id=_rule_id)
      GROUP BY r.id, r.project_id, r.name, r.type, deleted
  )
  SELECT row_to_json(t) AS rule FROM t;

$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION
  query_rule_table_associations(_id TEXT)
  RETURNS TEXT[] AS $$

  SELECT ARRAY(
    SELECT 'current' as TableName from iam_project_rules c where c.id=_id
    UNION
    SELECT 'staged' as TableName from iam_staged_project_rules a where a.id=_id
    );

$$ LANGUAGE sql;

COMMIT;
