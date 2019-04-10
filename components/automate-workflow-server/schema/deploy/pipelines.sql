-- Deploy pipelines
-- requires: projects

BEGIN;

CREATE TABLE IF NOT EXISTS pipelines(
  id BIGSERIAL PRIMARY KEY,
  project_id BIGINT NOT NULL REFERENCES projects(id) ON UPDATE CASCADE ON DELETE CASCADE,
  name TEXT NOT NULL,
  UNIQUE(project_id, name)
);

COMMIT;
