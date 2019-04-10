-- Revert rename_success

BEGIN;

UPDATE stage_runs SET status='successful' WHERE status='passed';
UPDATE phase_runs SET status='success' WHERE status='passed';

COMMIT;
