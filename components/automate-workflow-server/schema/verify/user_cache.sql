-- Verify user_cache

BEGIN;

SELECT id, cached_at
FROM user_cache
WHERE FALSE;

ROLLBACK;
