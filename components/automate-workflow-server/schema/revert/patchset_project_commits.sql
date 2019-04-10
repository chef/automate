-- Revert patchset_project_commits

BEGIN;

DROP TABLE IF EXISTS patchset_project_commits;

COMMIT;
