-- Verify delivery:phase_run_logs on pg

BEGIN;

SELECT id, run_id, data
  FROM phase_run_logs
  WHERE FALSE;

ROLLBACK;
