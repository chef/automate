BEGIN;

CREATE OR REPLACE FUNCTION
  query_roles(_projects_filter TEXT[])
  RETURNS setof json AS $$

  WITH t AS (
    SELECT
      r.id,
      r.name,
      r.type,
      r.actions,
      (SELECT array_agg(rp.project_id) FILTER (WHERE rp.project_id IS NOT NULL)) AS projects
    FROM iam_roles AS r
    LEFT OUTER JOIN iam_role_projects AS rp ON rp.role_id = r.db_id
    GROUP BY r.id, r.name, r.type, r.actions
  )
  SELECT
    json_build_object(
      'id', t.id,
      'name', t.name,
      'type', t.type,
      'actions', t.actions,
      'projects', COALESCE(t.projects, '{}')
    ) AS role FROM t
  WHERE projects_match(t.projects::TEXT[],  _projects_filter);

$$ LANGUAGE sql;

COMMIT;
