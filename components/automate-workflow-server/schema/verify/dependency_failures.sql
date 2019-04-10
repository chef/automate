-- Verify dependency_failures

BEGIN;

 SELECT
    enterprise_id,
    pipeline_id,
    grouping_id
    status
 FROM dependency_failures
 WHERE FALSE;

ROLLBACK;
