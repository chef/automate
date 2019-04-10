-- Verify delivery:oauth_tokens on pg

BEGIN;

SELECT id,
       oauth_app_id,
       scope,
       scope_id,
       state,
       token
FROM oauth_tokens
WHERE FALSE;

ROLLBACK;
