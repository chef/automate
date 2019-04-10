-- Revert stage_run_summary

BEGIN;

DROP FUNCTION IF EXISTS stage_run_summary(p_change_id changes.id%TYPE);

COMMIT;
