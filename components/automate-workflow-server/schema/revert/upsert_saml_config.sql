-- Deploy delivery:upsert_saml_config to pg

BEGIN;

-- We have to drop the previous function because it has a different # of parameters
-- If we do not drop this we will end up with two different functions with the same name.
DROP FUNCTION IF EXISTS upsert_saml_config(
  p_enterprise_name enterprises.name%TYPE,
  p_sso_login_url saml_config.sso_login_url%TYPE,
  p_sso_binding saml_config.sso_binding%TYPE,
  p_idp_url saml_config.idp_url%TYPE,
  p_cert saml_config.cert%TYPE,
  p_name_id saml_config.name_id%TYPE,
  p_metadata_url saml_config.metadata_url%TYPE,
  p_metadata_xml saml_config.metadata_xml%TYPE,
  p_default_roles saml_config.default_roles%TYPE
);

CREATE OR REPLACE FUNCTION upsert_saml_config(
  p_enterprise_name enterprises.name%TYPE,
  p_sso_login_url saml_config.sso_login_url%TYPE,
  p_sso_binding saml_config.sso_binding%TYPE,
  p_idp_url saml_config.idp_url%TYPE,
  p_cert saml_config.cert%TYPE,
  p_name_id saml_config.name_id%TYPE,
  p_metadata_url saml_config.metadata_url%TYPE,
  p_metadata_xml saml_config.metadata_xml%TYPE
)
RETURNS SETOF saml_config
ROWS 1
LANGUAGE plpgsql
AS $$
DECLARE
  v_enterprise_id enterprises.id%TYPE;
BEGIN

  -- lookup enterprise_id by name, or return exception
    SELECT id
    FROM enterprises
    WHERE name = p_enterprise_name
    INTO v_enterprise_id;

    IF NOT FOUND THEN
        RAISE EXCEPTION
        USING ERRCODE = 'CD003',
              MESSAGE = 'Enterprise not found',
              DETAIL  = 'Enterprise "' || p_enterprise_name || '" not found',
              HINT    = 'Make sure the enterprise exists, and the name is spelled correctly';
    END IF;

  -- try to insert
    RETURN QUERY
    INSERT INTO saml_config(
         sso_login_url,
         sso_binding,
         idp_url,
         cert,
         enterprise_id,
         name_id,
         metadata_url,
         metadata_xml)
      VALUES (p_sso_login_url,
              p_sso_binding,
              p_idp_url,
              p_cert,
              v_enterprise_id,
              p_name_id,
              p_metadata_url,
              p_metadata_xml)
      RETURNING saml_config.*;
    EXCEPTION WHEN unique_violation THEN

 -- Already a row for that enterprise so update
    RETURN QUERY
    UPDATE saml_config
    SET    sso_login_url = p_sso_login_url,
           sso_binding = p_sso_binding,
           idp_url = p_idp_url,
           cert = p_cert,
           name_id = p_name_id,
           metadata_url = p_metadata_url,
           metadata_xml = p_metadata_xml
    WHERE
           enterprise_id = v_enterprise_id
    RETURNING saml_config.*;

END;
$$;

COMMIT;
