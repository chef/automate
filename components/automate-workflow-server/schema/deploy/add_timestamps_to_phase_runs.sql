-- Deploy delivery:add_timestamps_to_phase_runs to pg
-- requires: phase_runs

BEGIN;

  -- created_at is added and has a default set in two steps because we want
  -- existing rows to have a null value, but new rows to have their timestamp
  -- set when they are created. If this was one alter table statement, existing
  -- rows would have their value set to the time the statement was run.
  ALTER TABLE phase_runs ADD COLUMN created_at cd_timestamp DEFAULT NULL;
  ALTER TABLE phase_runs ALTER COLUMN created_at SET DEFAULT NOW();

  ALTER TABLE phase_runs ADD COLUMN started_at cd_timestamp DEFAULT NULL;
  ALTER TABLE phase_runs ADD COLUMN finished_at cd_timestamp DEFAULT NULL;

COMMIT;
