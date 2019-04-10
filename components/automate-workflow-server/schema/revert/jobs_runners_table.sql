-- Revert delivery:jobs_runners_table from pg

BEGIN;

DROP TABLE jobs_runners;

COMMIT;
