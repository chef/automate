-- Verify delivery:external_oauth_applications on pg

BEGIN;

SELECT id,
       name,
       module,
       root_url,
       root_api_url,
       client_id,
       client_secret
FROM external_oauth_applications
WHERE FALSE;

ROLLBACK;
