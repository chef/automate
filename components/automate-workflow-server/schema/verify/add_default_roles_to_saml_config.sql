-- Verify delivery:add_default_roles_to_saml_config on pg

BEGIN;

SELECT id,
       enterprise_id,
       sso_login_url,
       sso_binding,
       idp_url,
       cert,
       name_id,
       metadata_url,
       metadata_xml,
       default_roles
  FROM saml_config
 WHERE FALSE;

ROLLBACK;
