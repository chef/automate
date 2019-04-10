-- Deploy delivery:oauth_integrations to pg
-- requires: external_oauth_applications
-- requires: oauth_tokens

BEGIN;

CREATE OR REPLACE VIEW oauth_integrations AS
SELECT a.id AS app_id,
       a.name AS app_name,
       a.module,
       a.root_url,
       a.root_api_url,
       a.client_id,
       a.client_secret,
       t.id AS token_id,
       t.scope,
       t.scope_id,
       t.state,
       t.token
FROM oauth_tokens AS t
JOIN external_oauth_applications AS a
ON t.oauth_app_id = a.id;

COMMENT ON VIEW oauth_integrations IS
$$
A view of all OAuth tokens joined with their OAuth application details.
$$;
COMMIT;
