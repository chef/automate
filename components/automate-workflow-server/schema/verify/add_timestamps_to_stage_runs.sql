-- Verify delivery:add_timestamps_to_stage_runs on pg

BEGIN;

  SELECT created_at, started_at, finished_at
    FROM stage_runs
   WHERE FALSE;

ROLLBACK;
