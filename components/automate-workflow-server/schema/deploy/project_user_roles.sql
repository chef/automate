-- Deploy project_user_roles
-- requires: projects, delivery_role

BEGIN;

CREATE TABLE IF NOT EXISTS project_user_roles(
  project_id BIGINT        NOT NULL REFERENCES projects(id) ON UPDATE CASCADE ON DELETE CASCADE,
  user_id    BIGINT        NOT NULL REFERENCES users(id)  ON UPDATE CASCADE ON DELETE CASCADE,
  role       delivery_role NOT NULL,

  PRIMARY KEY(project_id, user_id, role)
);

COMMIT;
