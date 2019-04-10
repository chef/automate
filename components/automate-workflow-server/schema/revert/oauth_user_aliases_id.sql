-- Revert delivery:oauth_user_aliases_id from pg

BEGIN;

ALTER TABLE IF EXISTS oauth_user_aliases DROP COLUMN IF EXISTS id;

COMMIT;
