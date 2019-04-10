-- Verify delivered_stats_on_change

BEGIN;

SELECT delivered_at, delivered_by
FROM changes
WHERE FALSE;

ROLLBACK;
