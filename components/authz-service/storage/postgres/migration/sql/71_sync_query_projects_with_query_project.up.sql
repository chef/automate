BEGIN;

CREATE OR REPLACE FUNCTION
  query_projects(_projects_filter TEXT[])
  RETURNS setof json AS $$

WITH
staged AS (
  SELECT r.project_id
  FROM iam_staged_project_rules AS r
  WHERE projects_match_for_rule(r.project_id, _projects_filter)
  GROUP BY r.project_id
),
applied AS (
  SELECT r.project_id
  FROM iam_project_rules AS r
  WHERE projects_match_for_rule(r.project_id, _projects_filter)
  GROUP BY r.project_id
)
SELECT json_build_object(
  'id', p.id,
  'name', p.name,
  'type', p.type,
  'status',
  CASE WHEN staged.project_id IS NULL AND applied.project_id IS NULL THEN 'no-rules'
       WHEN staged.project_id IS NULL THEN 'applied'
       ELSE 'edits-pending'
  END
)
FROM iam_projects AS p
LEFT JOIN applied
  ON applied.project_id = p.db_id
LEFT JOIN staged
  ON staged.project_id = p.db_id
WHERE projects_match_for_rule(p.id, _projects_filter)

$$ LANGUAGE sql;

COMMIT;
