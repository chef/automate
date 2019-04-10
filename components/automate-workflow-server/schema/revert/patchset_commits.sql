-- Revert patchset_commits

BEGIN;

DROP VIEW IF EXISTS patchset_commits;

COMMIT;
