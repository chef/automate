-- Revert delivery:oauth_user_aliases from pg

BEGIN;

DROP VIEW IF EXISTS user_aliases;
DROP TABLE IF EXISTS oauth_user_aliases;

COMMIT;
