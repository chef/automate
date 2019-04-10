-- Deploy delivery:add_name_id_saml_config to pg

BEGIN;

  ALTER TABLE saml_config ADD COLUMN name_id TEXT NOT NULL default 'urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress';

COMMIT;
