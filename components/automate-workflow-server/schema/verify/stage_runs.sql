-- Verify stage_runs

BEGIN;

SELECT id, change_id, stage, status, finished FROM stage_runs
  WHERE FALSE;

ROLLBACK;
