-- Verify user_tokens

BEGIN;

SELECT id, auth_token, auth_token_bday
FROM user_tokens
WHERE FALSE;

ROLLBACK;
