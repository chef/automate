BEGIN;

CREATE OR REPLACE FUNCTION
  query_rules_for_project(_project_id TEXT, _project_filter TEXT[])
  RETURNS setof json AS $$

  SELECT project_db_id(_project_id);

  -- fetch staged rules
  SELECT json_build_object(
    'id', r.id,
    'project_id', _project_id,
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
  INNER JOIN iam_staged_rule_conditions AS rc 
  ON rc.rule_db_id=r.db_id
  WHERE r.project_id=project_db_id(_project_id) AND 
        projects_match_for_rule(_project_id, _project_filter) AND
        r.deleted=false
  GROUP BY r.id, r.name, r.type

  UNION ALL

  -- fetch applied rules
  SELECT json_build_object(
    'id', r.id,
    'project_id', _project_id,
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
  WHERE r.project_id=project_db_id(_project_id) AND projects_match_for_rule(_project_id, _project_filter)
    -- return applied rule only if no staged changes exist
    AND NOT EXISTS (SELECT 1 FROM iam_staged_project_rules WHERE id=r.id)
  GROUP BY r.id, r.name, r.type

 $$ LANGUAGE sql;

-- dropping since the line below will just create a new function with new arguments
DROP FUNCTION query_staged_or_applied_rule(_rule_id TEXT, _project_filter TEXT[]);

CREATE OR REPLACE FUNCTION
  query_staged_or_applied_rule(_rule_id TEXT, _project_id TEXT, _project_filter TEXT[])
  RETURNS json AS $$

  SELECT project_db_id(_project_id);

  -- check for applied rule
  SELECT json_build_object(
    'id', r.id,
    'project_id', _project_id,
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
  INNER JOIN iam_staged_rule_conditions AS rc
  ON rc.rule_db_id=r.db_id
  WHERE r.project_id=project_db_id(_project_id) AND
        r.id=_rule_id AND
        projects_match_for_rule(r.project_id, _project_filter)
  GROUP BY r.id, r.name, r.type, r.deleted

  UNION ALL

  -- check for applied rule
  SELECT json_build_object(
    'id', r.id,
    'project_id', _project_id,
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
  INNER JOIN iam_rule_conditions AS rc
   ON rc.rule_db_id=r.db_id
  WHERE r.project_id=project_db_id(_project_id) AND
        r.id=_rule_id AND
        projects_match_for_rule(r.project_id, _project_filter)
  GROUP BY r.id, r.name, r.type;

$$ LANGUAGE sql;
 

CREATE OR REPLACE FUNCTION
  query_rule_table_associations(_id TEXT, _project_id TEXT)
  RETURNS TEXT[] AS $$

  -- confirm project exists
  SELECT project_db_id(_project_id);

  SELECT ARRAY(
    SELECT 'applied' AS TableName FROM iam_project_rules a WHERE a.id=_id
    UNION
    SELECT 'staged' AS TableName FROM iam_staged_project_rules s WHERE s.id=_id
    );

$$ LANGUAGE sql;

COMMIT;
