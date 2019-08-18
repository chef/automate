BEGIN;

CREATE OR REPLACE FUNCTION
  query_projects(_projects_filter TEXT[])
  RETURNS setof json AS $$

WITH
staged AS (
  SELECT r.project_id, count(r.id) AS count
  FROM iam_staged_project_rules AS r
  WHERE projects_match_for_rule(r.project_id, _projects_filter) AND r.deleted = false
  GROUP BY r.project_id
),
applied AS (
  SELECT r.project_id, count(r.id) AS count
  FROM iam_project_rules AS r
  WHERE projects_match_for_rule(r.project_id, _projects_filter)
  GROUP BY r.project_id
)
SELECT json_build_object(
  'id', p.id,
  'name', p.name,
  'type', p.type,
  'status', 
  CASE WHEN staged.count IS NULL AND applied.count IS NULL THEN 'no-rules'
       WHEN staged.count IS NULL THEN 'applied'
       ELSE 'edits-pending'
  END
)
FROM staged
FULL JOIN applied
ON staged.project_id = applied.project_id
RIGHT JOIN iam_projects p
ON p.db_id = COALESCE(staged.project_id, applied.project_id)
WHERE projects_match_for_rule(p.id, _projects_filter)

$$ LANGUAGE sql;

COMMIT;

