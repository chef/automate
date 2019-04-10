-- Verify delivery:user_oauth_aliases on pg

BEGIN;

SELECT user_id,
       oauth_app_id,
       alias
FROM oauth_user_aliases
WHERE FALSE;

SELECT id,
       enterprise_id,
       name,
       ssh_pub_key,
       first_name,
       last_name,
       email,
       user_type,
       oauth_app_id,
       alias
FROM user_aliases
WHERE FALSE;

ROLLBACK;
