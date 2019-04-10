-- Verify changesets

BEGIN;

SELECT id,
       pipeline_id,
       delivered_at,
       delivered_by
FROM changesets
WHERE FALSE;

ROLLBACK;
