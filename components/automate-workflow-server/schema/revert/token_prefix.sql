-- Revert token_prefix

BEGIN;

DROP DOMAIN IF EXISTS token_prefix;

COMMIT;
