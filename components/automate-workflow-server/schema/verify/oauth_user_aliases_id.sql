-- Verify delivery:oauth_user_aliases_id on pg

BEGIN;

SELECT id
FROM oauth_user_aliases
WHERE FALSE;

ROLLBACK;
