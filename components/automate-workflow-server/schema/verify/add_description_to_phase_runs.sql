-- Verify delivery:add_description_to_phase_runs on pg

BEGIN;

SELECT description
FROM phase_runs
WHERE false;

ROLLBACK;
