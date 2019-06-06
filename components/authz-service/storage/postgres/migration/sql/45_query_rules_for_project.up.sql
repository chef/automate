BEGIN;

CREATE OR REPLACE FUNCTION
  query_rules_for_project(_project_id TEXT, _project_filter TEXT[])
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
    WHERE _project_id=project_id AND projects_match_for_rule(project_id, _project_filter)
    GROUP BY r.id, r.project_id, r.name, r.type
  )
  SELECT row_to_json(t) AS rule FROM t;

$$ LANGUAGE sql;

COMMIT;
