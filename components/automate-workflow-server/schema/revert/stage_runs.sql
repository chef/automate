-- Revert stage_runs

BEGIN;

DROP TABLE IF EXISTS stage_runs;

COMMIT;
