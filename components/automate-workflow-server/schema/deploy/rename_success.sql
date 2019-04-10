-- Deploy rename_success

BEGIN;

UPDATE stage_runs SET status='passed' WHERE status='success' OR status='successful';
UPDATE phase_runs SET status='passed' WHERE status='success' OR status='successful';

COMMIT;
