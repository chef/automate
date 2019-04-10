-- Revert user_tokens

BEGIN;

DROP TABLE IF EXISTS user_tokens;

COMMIT;
