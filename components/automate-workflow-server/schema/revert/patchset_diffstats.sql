-- Revert patchset_diffstats

BEGIN;

DROP TABLE IF EXISTS patchset_diffstats;

COMMIT;
