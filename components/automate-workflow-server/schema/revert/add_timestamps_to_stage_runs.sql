-- Revert delivery:add_timestamps_to_stage_runs from pg

BEGIN;

  ALTER TABLE stage_runs DROP COLUMN created_at;
  ALTER TABLE stage_runs DROP COLUMN started_at;
  ALTER TABLE stage_runs DROP COLUMN finished_at;

COMMIT;
