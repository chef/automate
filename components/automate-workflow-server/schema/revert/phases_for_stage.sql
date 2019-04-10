-- Revert phases_for_stage

BEGIN;

DROP FUNCTION IF EXISTS phases_for_stage(stage_runs.stage%TYPE);

COMMIT;
