-- Revert delivery:scanner_add_results_table from pg

BEGIN;

DROP TABLE IF EXISTS results;

COMMIT;
