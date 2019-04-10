-- Revert delivery:password_reset_tokens from pg

BEGIN;

DROP TABLE IF EXISTS password_reset_tokens;

COMMIT;
