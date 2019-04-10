-- Revert user_passwords

BEGIN;

DROP TABLE IF EXISTS user_passwords;

COMMIT;
