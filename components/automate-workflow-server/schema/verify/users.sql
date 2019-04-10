-- Verify users

BEGIN;

SELECT id,
       enterprise_id,
       name,
       ssh_pub_key,
       first_name,
       last_name,
       email,
       user_type
    FROM users WHERE FALSE;

ROLLBACK;
