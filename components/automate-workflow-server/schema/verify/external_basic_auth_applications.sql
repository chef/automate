-- Verify delivery:external_basic_auth_applications on pg

BEGIN;

SELECT id,
       name,
       root_api_url,
       user_id,
       password
FROM external_basic_auth_applications
WHERE FALSE;

ROLLBACK;
