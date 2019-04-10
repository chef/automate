-- Revert delivery:add_name_id_saml_config from pg

BEGIN;

  ALTER TABLE saml_config DROP COLUMN name_id;

COMMIT;
