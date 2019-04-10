-- Revert delivery:add_description_to_phase_runs from pg

BEGIN;

ALTER TABLE phase_runs DROP COLUMN description;

COMMIT;
