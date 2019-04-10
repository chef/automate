-- Revert delivery:add_timestamps_to_phase_runs from pg

BEGIN;

  ALTER TABLE phase_runs DROP COLUMN created_at;
  ALTER TABLE phase_runs DROP COLUMN started_at;
  ALTER TABLE phase_runs DROP COLUMN finished_at;

COMMIT;
