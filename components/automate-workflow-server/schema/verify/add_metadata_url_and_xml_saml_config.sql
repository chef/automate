-- Verify delivery:add_metadata_url_and_xml_saml_config on pg

BEGIN;

SELECT id,
       enterprise_id,
       sso_login_url,
       sso_binding,
       idp_url,
       cert,
       name_id,
       metadata_url,
       metadata_xml
  FROM saml_config
 WHERE FALSE;

ROLLBACK;
