-- Verify delivery:oauth_integrations on pg

BEGIN;

SELECT app_id,
       app_name,
       module,
       root_url,
       root_api_url,
       client_id,
       client_secret,
       token_id,
       scope,
       scope_id,
       state
       token
FROM oauth_integrations
WHERE FALSE;

ROLLBACK;
