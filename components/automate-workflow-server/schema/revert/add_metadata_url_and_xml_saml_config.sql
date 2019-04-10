-- Revert delivery:add_metadata_url_and_xml_saml_config from pg

BEGIN;

  ALTER TABLE saml_config
    DROP COLUMN metadata_url,
    DROP COLUMN metadata_xml;

COMMIT;
