BEGIN;

ALTER TABLE iam_roles ADD CONSTRAINT iam_roles_pkey PRIMARY KEY (db_id);

CREATE TABLE iam_role_projects (
  role_id INTEGER REFERENCES iam_roles ON DELETE CASCADE,
  project_id TEXT REFERENCES iam_projects ON DELETE CASCADE,
  PRIMARY KEY (role_id, project_id)
);

CREATE OR REPLACE FUNCTION
  query_role(_role_id TEXT)
  RETURNS json AS $$

  WITH t AS
    (SELECT r.id, r.name, r.type, r.actions, (SELECT COALESCE(json_agg(rp.project_id) FILTER (WHERE rp.project_id IS NOT NULL), '[]')) AS projects
      FROM iam_roles AS r
      LEFT OUTER JOIN iam_role_projects AS rp ON rp.role_id=r.db_id
      WHERE r.id = _role_id
      GROUP BY r.id, r.name, r.type, r.actions)
  SELECT row_to_json(t) AS role FROM t;

$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION
  query_roles()
  RETURNS setof json AS $$

  WITH t AS
    (SELECT r.id, r.name, r.type, r.actions, (SELECT COALESCE(json_agg(rp.project_id) FILTER (WHERE rp.project_id IS NOT NULL), '[]')) AS projects
      FROM iam_roles AS r
      LEFT OUTER JOIN iam_role_projects AS rp ON rp.role_id=r.db_id
      GROUP BY r.id, r.name, r.type, r.actions)
  SELECT row_to_json(t) AS role FROM t;

$$ LANGUAGE sql;

COMMIT;
