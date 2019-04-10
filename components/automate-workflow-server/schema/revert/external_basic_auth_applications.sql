-- Revert delivery:external_basic_auth_applications from pg

BEGIN;

DROP TABLE IF EXISTS external_basic_auth_applications;

COMMIT;
