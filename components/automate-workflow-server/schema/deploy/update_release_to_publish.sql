-- Deploy update_release_to_publish

BEGIN;

UPDATE stage_ordering SET stage='build' WHERE stage='release';
UPDATE stage_ordering SET phase='publish' WHERE phase='release';
UPDATE stage_runs SET stage='build' WHERE stage='release';
UPDATE phase_runs SET phase='publish' WHERE phase='release';

COMMIT;
