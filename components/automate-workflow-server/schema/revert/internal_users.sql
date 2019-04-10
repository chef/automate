-- Revert internal_users

BEGIN;

DROP VIEW IF EXISTS internal_users;

COMMIT;
