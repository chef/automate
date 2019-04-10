-- Revert scoped_stage_runs_view

BEGIN;

DROP VIEW IF EXISTS scoped_stage_runs;

COMMIT;
