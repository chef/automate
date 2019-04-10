-- Revert stage_runs_to_restart_view

BEGIN;

DROP VIEW IF EXISTS stage_runs_to_restart;

COMMIT;
