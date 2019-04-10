-- Revert delivery:add_saml_config_table from pg

BEGIN;

    DROP TABLE IF EXISTS saml_config;

COMMIT;
