BEGIN;

CREATE OR REPLACE FUNCTION
  query_rules_for_project(_project_id TEXT, _project_filter TEXT[])
  RETURNS setof json AS $$

  -- fetch staged rules
   SELECT json_build_object(
    'id', r.id,
    'project_id', r.project_id,
    'name', r.name,
    'type', r.type,
    'status', 'staged',
    'conditions', json_agg(
      json_build_object(
        'value', rc.value,
        'operator', rc.operator,
        'attribute', rc.attribute
      )
    )
  ) AS rule
  FROM iam_staged_project_rules AS r
  INNER JOIN iam_staged_rule_conditions AS rc ON rc.rule_db_id=r.db_id
  WHERE projects_match_for_rule(project_id, _project_filter) AND r.deleted=false
  GROUP BY r.id, r.project_id, r.name, r.type

  UNION ALL

  -- fetch applied rules
  SELECT json_build_object(
    'id', r.id,
    'project_id', r.project_id,
    'name', r.name,
    'type', r.type,
    'status', 'applied',
    'conditions', json_agg(
      json_build_object(
        'value', rc.value,
        'operator', rc.operator,
        'attribute', rc.attribute
      )
    )
  ) AS rule
  FROM iam_project_rules AS r
  INNER JOIN iam_rule_conditions AS rc
  ON rc.rule_db_id=r.db_id
  WHERE _project_id=project_id AND projects_match_for_rule(project_id, _project_filter)
    -- return applied rule only if no staged changes exist
    AND NOT EXISTS (SELECT 1 FROM iam_staged_project_rules WHERE id=r.id)
  GROUP BY r.id, r.project_id, r.name, r.type;

 $$ LANGUAGE sql;

 COMMIT;
