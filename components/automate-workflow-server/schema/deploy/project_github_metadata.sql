-- Deploy project_github_metadata
-- requires: projects

BEGIN;

CREATE TABLE IF NOT EXISTS project_github_metadata(
  project_id BIGINT NOT NULL PRIMARY KEY REFERENCES projects(id) ON UPDATE CASCADE ON DELETE CASCADE,
  -- the repo's owner (org or user) and name together are what uniquely
  -- identify a github project (e.g. chef/delivery)
  repo_owner TEXT NOT NULL,
  repo_name TEXT NOT NULL,
  -- a given github repo can be associated with at most one Delivery project
  UNIQUE(repo_owner, repo_name),
  token TEXT NOT NULL
);

COMMIT;
