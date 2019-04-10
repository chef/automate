-- Revert user

BEGIN;

DROP TABLE IF EXISTS users;

COMMIT;
