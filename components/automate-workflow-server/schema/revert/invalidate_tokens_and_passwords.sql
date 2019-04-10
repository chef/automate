-- Revert invalidate_tokens_and_passwords

BEGIN;

DROP FUNCTION IF EXISTS invalidate(credential, enterprises.name%TYPE, users.name%TYPE);
DROP FUNCTION IF EXISTS invalidate(credential, enterprises.name%TYPE);
DROP FUNCTION IF EXISTS invalidate(credential);

DROP FUNCTION IF EXISTS credential_table(credential);
DROP TYPE IF EXISTS credential;

COMMIT;
