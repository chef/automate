-- Deploy delivery:add_search_desc_to_phase_runs to pg

BEGIN;

ALTER TABLE phase_runs ADD COLUMN search_description TEXT;

COMMIT;
