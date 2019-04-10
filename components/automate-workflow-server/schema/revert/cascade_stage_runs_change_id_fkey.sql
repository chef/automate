-- Revert cascade_stage_runs_change_id_fkey

BEGIN;

ALTER TABLE stage_runs DROP CONSTRAINT IF EXISTS stage_runs_change_id_fkey;
ALTER TABLE stage_runs ADD CONSTRAINT stage_runs_change_id_fkey FOREIGN KEY (change_id) REFERENCES changes(id);

COMMIT;
