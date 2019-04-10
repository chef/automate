-- Revert delivery:github_patchsets from pg

BEGIN;

DROP TABLE IF EXISTS github_patchsets;

COMMIT;
