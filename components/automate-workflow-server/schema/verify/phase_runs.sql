-- Verify phase_runs

BEGIN;

SELECT id, stage_run_id, phase, status,
       finished, run_success, run_log, run_status,
       build_node, search_query
  FROM phase_runs
  WHERE FALSE;

ROLLBACK;
