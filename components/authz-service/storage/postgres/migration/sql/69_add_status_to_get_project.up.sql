BEGIN;

CREATE OR REPLACE FUNCTION
  query_project(
    _project_id TEXT,
    _projects_filter TEXT[]
  )
  RETURNS json AS $$
declare
  project_json json;

BEGIN
  WITH staged AS (
    SELECT r.project_id
      FROM iam_staged_project_rules AS r
      WHERE project_db_id(_project_id) = r.project_id
      GROUP BY r.project_id
  ),
  applied AS (
    SELECT  r.project_id
      FROM iam_project_rules AS r
      WHERE project_db_id(_project_id) = r.project_id
    GROUP BY r.project_id
  )
  SELECT
  json_build_object(
    'id', p.id,
    'name', p.name,
    'type', p.type,
    'status',
    CASE WHEN staged.project_id IS NULL AND applied.project_id IS NULL THEN 'no-rules'
      WHEN staged.project_id IS NULL THEN 'applied'
      ELSE 'edits-pending'
    END
  ) INTO STRICT project_json
  FROM iam_projects AS p
  LEFT JOIN applied
    ON applied.project_id = p.db_id
  LEFT JOIN staged
    ON staged.project_id = p.db_id
  WHERE p.id=_project_id AND (array_length(_projects_filter, 1) IS NULL OR p.id=ANY(_projects_filter));

  RETURN project_json;
END;
$$ LANGUAGE plpgsql;

COMMIT;

