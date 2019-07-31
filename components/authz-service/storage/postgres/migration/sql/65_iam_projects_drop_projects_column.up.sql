BEGIN;

ALTER TABLE iam_projects DROP COLUMN projects;

CREATE OR REPLACE FUNCTION
  query_projects(_projects_filter TEXT[])
  RETURNS setof json AS $$

  SELECT
    json_build_object(
      'id', id,
      'name', name,
      'type', type
    )
  FROM iam_projects
  WHERE array_length(_projects_filter, 1) IS NULL OR id=ANY(_projects_filter);

$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION
  query_project(
    _project_id TEXT,
    _projects_filter TEXT[],
    OUT _project json
  )
  RETURNS json AS $$

BEGIN
  SELECT
    json_build_object(
      'id', id,
      'name', name,
      'type', type
    ) INTO STRICT _project
  FROM iam_projects
  WHERE id=_project_id AND (array_length(_projects_filter, 1) IS NULL OR id=ANY(_projects_filter));
END;
$$ LANGUAGE plpgsql;

COMMIT;
