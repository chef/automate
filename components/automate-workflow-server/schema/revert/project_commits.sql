-- Revert project_commits

BEGIN;

DROP TABLE IF EXISTS project_commits;

COMMIT;
