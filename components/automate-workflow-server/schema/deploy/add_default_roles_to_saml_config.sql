-- Deploy delivery:add_default_roles_to_saml_config to pg

BEGIN;

  ALTER TABLE saml_config
    ADD COLUMN default_roles text[];

COMMIT;
