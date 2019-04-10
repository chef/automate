-- Revert user_cache

BEGIN;

DROP TABLE IF EXISTS user_cache;

COMMIT;
