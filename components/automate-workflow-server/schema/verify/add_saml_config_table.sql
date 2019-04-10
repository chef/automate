-- Verify delivery:add_saml_config_table on pg

BEGIN;

SELECT id,
       enterprise_id,
       sso_login_url,
       sso_binding,
       idp_url,
       cert
  FROM saml_config
 WHERE FALSE;

ROLLBACK;
