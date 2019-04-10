-- Revert hash_type

BEGIN;

DROP TYPE password_hash_type;

COMMIT;
