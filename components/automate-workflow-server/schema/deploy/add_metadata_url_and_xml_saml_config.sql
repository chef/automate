-- Deploy delivery:add_metadata_url_and_xml_saml_config to pg

BEGIN;

  ALTER TABLE saml_config
    ADD COLUMN metadata_xml TEXT,
    ADD COLUMN metadata_url TEXT;

  /* if the user gives us a metadata, these values will be read from there */
  ALTER TABLE saml_config
    ALTER COLUMN sso_login_url DROP NOT NULL,
    ALTER COLUMN sso_binding DROP NOT NULL,
    ALTER COLUMN idp_url DROP NOT NULL,
    ALTER COLUMN cert DROP NOT NULL;

COMMIT;
