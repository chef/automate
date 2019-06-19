BEGIN;

CREATE OR REPLACE FUNCTION
  query_staged_and_applied_rules(_project_filter TEXT[])
  RETURNS setof json AS $$

  -- fetch staged rules
  SELECT json_build_object(
    'id', r.id,
    'project_id', r.project_id,
    'name', r.name,
    'type', r.type,
    'deleted', r.deleted,
    'status', 'staged',
    'conditions', json_agg(
      json_build_object(
        'value', rc.value,
        'operator', rc.operator,
        'attribute', rc.attribute
      )
    )
  ) AS rules
  FROM iam_staged_project_rules AS r
  INNER JOIN iam_staged_rule_conditions AS rc ON rc.rule_db_id=r.db_id
  WHERE projects_match_for_rule(project_id, _project_filter)
  GROUP BY r.id, r.project_id, r.name, r.type, r.deleted

  UNION ALL

  -- fetch applied rules
  SELECT json_build_object(
    'id', r.id,
    'project_id', r.project_id,
    'name', r.name,
    'type', r.type,
    'deleted', false,
    'status', 'applied',
    'conditions', json_agg(
      json_build_object(
        'value', rc.value,
        'operator', rc.operator,
        'attribute', rc.attribute
      )
    )
  ) AS rules
  FROM iam_project_rules AS r
  INNER JOIN iam_rule_conditions AS rc ON rc.rule_db_id=r.db_id
  WHERE projects_match_for_rule(project_id, _project_filter)
  GROUP BY r.id, r.project_id, r.name, r.type;

$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION
  query_staged_rule(_rule_id TEXT, _project_filter TEXT[])
  RETURNS json AS $$

  -- check for applied rule
  SELECT json_build_object(
    'id', r.id,
    'project_id', r.project_id,
    'name', r.name,
    'type', r.type,
    'deleted', r.deleted,
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
  WHERE id=_rule_id AND projects_match_for_rule(project_id, _project_filter)
  GROUP BY r.id, r.project_id, r.name, r.type, r.deleted

  UNION ALL

  -- check for applied rule
  SELECT json_build_object(
    'id', r.id,
    'project_id', r.project_id,
    'name', r.name,
    'type', r.type,
    'deleted', false,
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
  INNER JOIN iam_rule_conditions AS rc ON rc.rule_db_id=r.db_id
  WHERE id=_rule_id AND projects_match_for_rule(project_id, _project_filter)
  GROUP BY r.id, r.project_id, r.name, r.type;

$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION
  query_rules(_project_filter TEXT[])
  RETURNS setof json AS $$

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
  INNER JOIN iam_rule_conditions AS rc ON rc.rule_db_id=r.db_id
  WHERE projects_match_for_rule(project_id, _project_filter)
  GROUP BY r.id, r.project_id, r.name, r.type;

$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION
  query_rules_for_project(_project_id TEXT, _project_filter TEXT[])
  RETURNS setof json AS $$

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
  GROUP BY r.id, r.project_id, r.name, r.type;

$$ LANGUAGE sql;

COMMIT;
