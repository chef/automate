BEGIN;
-- break all references
ALTER TABLE iam_role_projects
    DROP CONSTRAINT iam_role_projects_project_id_fkey;

ALTER TABLE iam_role_projects RENAME COLUMN project_id TO project_temp_id;
ALTER TABLE iam_role_projects
    ADD COLUMN project_id INTEGER REFERENCES iam_projects (db_id) ON DELETE CASCADE DEFERRABLE;
UPDATE
    iam_role_projects t
SET
    project_id = (
        SELECT
            db_id
        FROM
            iam_projects
        WHERE
            id = t.project_temp_id);
ALTER TABLE iam_role_projects
    DROP COLUMN project_temp_id;

CREATE OR REPLACE FUNCTION
  query_role(_role_id TEXT)
  RETURNS json AS $$

  SELECT json_build_object(
    'id', r.id,
    'name', r.name,
    'type', r.type,
    'actions', r.actions,
    'projects', COALESCE(json_agg(p.id) FILTER (WHERE p.id IS NOT NULL), '[]')
    ) AS role
  FROM iam_roles AS r
  LEFT JOIN iam_role_projects AS rp
  ON rp.role_id=r.db_id
  LEFT JOIN iam_projects AS p
  ON rp.project_id=p.db_id
  WHERE r.id=_role_id
  GROUP BY r.id, r.name, r.type, r.actions

$$ LANGUAGE sql;

CREATE FUNCTION
  role_projects(_role_id TEXT)
  RETURNS TEXT[] AS $$

  SELECT COALESCE(array_agg(p.id) FILTER (WHERE p.id IS NOT NULL), '{(unassigned)}')
  FROM iam_roles AS r
  LEFT JOIN iam_role_projects AS rp
  ON rp.role_id=r.db_id
  LEFT JOIN iam_projects AS p
  ON rp.project_id=p.db_id
  WHERE r.id=_role_id

$$ LANGUAGE sql;

COMMIT;
