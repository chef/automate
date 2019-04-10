-- Deploy patchset_project_commits
-- requires: patchsets, project_commits

BEGIN;

CREATE TABLE IF NOT EXISTS patchset_project_commits(
  patchset_id BIGINT NOT NULL REFERENCES patchsets(id) ON UPDATE CASCADE ON DELETE CASCADE,
  project_commit_id BIGINT NOT NULL REFERENCES project_commits(id) ON UPDATE CASCADE ON DELETE CASCADE,
  UNIQUE(patchset_id, project_commit_id)
);

COMMENT ON TABLE patchset_project_commits IS
$$Pivot table between patchsets and project_commits
$$;

COMMIT;
