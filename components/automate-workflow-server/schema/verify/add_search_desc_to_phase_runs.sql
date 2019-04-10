-- Verify delivery:add_search_desc_to_phase_runs on pg

BEGIN;

SELECT search_description
  FROM phase_runs
 WHERE FALSE;
 
ROLLBACK;
