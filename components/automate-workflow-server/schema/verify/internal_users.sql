-- Verify internal_users

BEGIN;

SELECT id,
       enterprise_id,
       name,
       ssh_pub_key,
       first_name,
       last_name,
       email,
       hashed_pass,
       hash_type
FROM internal_users
WHERE FALSE;

ROLLBACK;
