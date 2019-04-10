-- Revert delivery:add_default_roles_to_saml_config from pg

BEGIN;

  ALTER TABLE saml_config
    DROP COLUMN default_roles;

COMMIT;
