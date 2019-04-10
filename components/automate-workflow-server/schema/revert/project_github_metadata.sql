-- Revert project_github_metadata

BEGIN;

DROP TABLE IF EXISTS project_github_metadata;

COMMIT;
