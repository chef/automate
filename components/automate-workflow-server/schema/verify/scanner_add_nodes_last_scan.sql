-- Verify delivery:scanner_add_nodes_last_scan on pg

BEGIN;

SELECT last_job
FROM nodes WHERE FALSE;

ROLLBACK;
