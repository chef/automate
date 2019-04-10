-- Verify delivery:add_enterprise_to_jobs_runners on pg

BEGIN;

SELECT enterprise_id
FROM jobs_runners
WHERE FALSE;

ROLLBACK;
