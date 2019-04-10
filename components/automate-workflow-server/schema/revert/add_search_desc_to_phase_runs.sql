-- Revert delivery:add_search_desc_to_phase_runs from pg

BEGIN;

ALTER TABLE phase_runs DROP COLUMN search_description;

COMMIT;
