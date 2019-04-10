-- Revert update_internal_user

BEGIN;

DROP FUNCTION IF EXISTS update_internal_user();

COMMIT;
