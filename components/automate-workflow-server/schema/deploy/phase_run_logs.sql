-- Deploy delivery:phase_run_logs to pg
-- requires: phase_runs

BEGIN;

CREATE TABLE IF NOT EXISTS phase_run_logs (
  id UUID NOT NULL PRIMARY KEY DEFAULT uuid_generate_v4(),
  run_id BIGINT NOT NULL REFERENCES phase_runs(id) ON DELETE CASCADE,
  data TEXT
);

COMMIT;
