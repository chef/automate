-- Revert delivery:external_oauth_applications from pg

BEGIN;

DROP TABLE IF EXISTS external_oauth_applications;

COMMIT;
