-- Revert delete_internal_user

BEGIN;

DROP FUNCTION IF EXISTS delete_internal_user();

COMMIT;
