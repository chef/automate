-- Revert changesets

BEGIN;

DROP TABLE IF EXISTS changesets CASCADE;

COMMIT;
