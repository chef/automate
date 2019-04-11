BEGIN;

DROP FUNCTION IF EXISTS query_roles();

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
  WHERE
    -- if empty _projects_filter is passed, return row
    array_length(_projects_filter, 1) IS NULL
    -- if _projects_filter intersects with projects list for row, return row
    OR t.projects && _projects_filter
    -- if projects list for row is empty, set it to'{(unassigned)}'
    -- and see if it intersects with _projects_filter, if so, return row
    OR COALESCE(t.projects, '{(unassigned)}') && _projects_filter;

$$ LANGUAGE sql;

COMMIT;
