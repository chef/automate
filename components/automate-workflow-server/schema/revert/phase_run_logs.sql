-- Revert delivery:phase_run_logs from pg

BEGIN;

DROP TABLE IF EXISTS phase_run_logs;

COMMIT;
