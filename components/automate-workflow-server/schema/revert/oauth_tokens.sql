-- Revert delivery:oauth_tokens from pg

BEGIN;

DROP TABLE IF EXISTS oauth_tokens;

COMMIT;
