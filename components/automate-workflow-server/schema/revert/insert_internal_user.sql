-- Revert insert_internal_user

BEGIN;

DROP FUNCTION IF EXISTS insert_internal_user();

COMMIT;
