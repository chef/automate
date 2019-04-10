-- Revert update_release_to_publish

BEGIN;

UPDATE stage_ordering SET stage='release' WHERE stage='build';
UPDATE stage_ordering SET phase='release' WHERE phase='publish';
UPDATE stage_runs SET stage='release' WHERE stage='build';
UPDATE phase_runs SET phase='release' WHERE phase='publish';

COMMIT;
