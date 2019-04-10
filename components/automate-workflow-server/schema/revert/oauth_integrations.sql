-- Revert delivery:oauth_integrations from pg

BEGIN;

DROP VIEW IF EXISTS oauth_integrations;

COMMIT;
