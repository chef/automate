-- Verify delivery:add_timestamps_to_phase_runs on pg

BEGIN;

  SELECT created_at, started_at, finished_at
    FROM phase_runs
   WHERE FALSE;

ROLLBACK;
