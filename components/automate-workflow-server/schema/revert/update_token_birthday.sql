-- Revert update_token_birthday

BEGIN;

DROP FUNCTION IF EXISTS update_token_birthday();

COMMIT;
