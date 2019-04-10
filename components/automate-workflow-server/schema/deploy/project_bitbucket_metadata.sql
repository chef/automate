-- Deploy delivery:project_bitbucket_metadata to pg
-- requires: projects

BEGIN;

CREATE TABLE IF NOT EXISTS project_bitbucket_metadata(
  project_id BIGINT NOT NULL PRIMARY KEY REFERENCES projects(id) ON UPDATE CASCADE ON DELETE CASCADE,
  -- the repo's project and name together are what uniquely
  -- identify a bitbucket project (e.g. chef/delivery)
  bitbucket_project TEXT NOT NULL,
  repo_name TEXT NOT NULL,
  -- a given bitbucekt repo can be associated with at most one Delivery project
  UNIQUE(bitbucket_project, repo_name)
);

COMMENT ON TABLE project_bitbucket_metadata IS
'Extra metadata attached to bitbucket projects. This is used to link the Delivery
 project to the bitbucket project.';

COMMIT;
