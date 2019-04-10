-- Revert update_comment

BEGIN;

DROP FUNCTION IF EXISTS update_comment();

COMMIT;
