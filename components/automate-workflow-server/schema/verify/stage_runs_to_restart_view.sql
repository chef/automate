-- Verify stage_runs_to_restart_view

BEGIN;

SELECT id,
       change_id,
       stage,
       status,
       finished
FROM stage_runs_to_restart
WHERE false;

ROLLBACK;
