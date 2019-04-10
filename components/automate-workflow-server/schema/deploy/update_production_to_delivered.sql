-- Deploy update_production_to_delivered

BEGIN;

UPDATE stage_ordering SET stage='delivered' WHERE stage='production';
UPDATE stage_runs SET stage='delivered' WHERE stage='production';

COMMIT;
