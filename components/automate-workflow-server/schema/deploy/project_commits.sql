-- Deploy project_commits
-- requires: projects

BEGIN;

CREATE TABLE IF NOT EXISTS project_commits(
  id BIGSERIAL PRIMARY KEY,
  project_id BIGINT NOT NULL REFERENCES projects(id) ON UPDATE CASCADE ON DELETE CASCADE,
  sha TEXT NOT NULL,
  UNIQUE(project_id, sha),
  subject TEXT NOT NULL,
  body TEXT NOT NULL
);

COMMENT ON TABLE project_commits IS
$$This table caches the git data for each
individual commit that is part of at least one
patchset for a given project.
TODO: we might want to GC this table from time to time?
$$;

COMMIT;
