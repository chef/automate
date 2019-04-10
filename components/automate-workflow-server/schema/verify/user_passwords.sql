-- Verify user_passwords

BEGIN;

SELECT id, hashed_pass, hash_type
FROM user_passwords
WHERE FALSE;

ROLLBACK;
