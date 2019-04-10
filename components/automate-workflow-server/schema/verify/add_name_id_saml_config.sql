-- Verify delivery:add_name_id_saml_config on pg

BEGIN;

SELECT id,
       enterprise_id,
       sso_login_url,
       sso_binding,
       idp_url,
       cert,
       name_id
  FROM saml_config
 WHERE FALSE;

ROLLBACK;
