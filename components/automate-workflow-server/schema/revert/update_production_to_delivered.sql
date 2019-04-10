-- Revert update_production_to_delivered

BEGIN;

UPDATE stage_ordering SET stage='production' WHERE stage='delivered';
UPDATE stage_runs SET stage='production' WHERE stage='delivered';

COMMIT;
