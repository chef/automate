-- Revert raise_user_type_change_exception

BEGIN;

DROP FUNCTION IF EXISTS raise_user_type_change_exception();

COMMIT;
