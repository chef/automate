-- Revert delivery:upsert_saml_config from pg

BEGIN;

DROP FUNCTION IF EXISTS upsert_saml_config(
    p_enterprise_name enterprises.name%TYPE,
    p_sso_login_url saml_config.sso_login_url%TYPE,
    p_sso_binding saml_config.sso_binding%TYPE,
    p_idp_url saml_config.idp_url%TYPE,
    p_cert saml_config.cert%TYPE 

);

COMMIT;
