-- Verify delivery:password_reset_tokens on pg

BEGIN;

SELECT user_id, token, expiry
FROM password_reset_tokens
WHERE FALSE;

ROLLBACK;
