-- Verify delivery:scanner_add_results_table on pg

BEGIN;

SELECT
    job_id,
    node_id,
    report_id,
    status,
    result,
    start_time,
    end_time
FROM results WHERE FALSE;

ROLLBACK;
