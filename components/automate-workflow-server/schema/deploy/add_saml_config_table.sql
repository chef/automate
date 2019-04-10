-- Deploy delivery:add_saml_config_table to pg

BEGIN;

CREATE TABLE IF NOT EXISTS saml_config(
    id BIGSERIAL PRIMARY KEY,
    enterprise_id BIGINT NOT NULL REFERENCES enterprises(id) ON UPDATE CASCADE ON DELETE CASCADE,
    sso_login_url TEXT NOT NULL,
    sso_binding TEXT NOT NULL,
    idp_url TEXT  NOT NULL,
    cert TEXT NOT NULL
);

CREATE UNIQUE INDEX saml_config_enterprise_key
    ON saml_config (enterprise_id);

COMMIT;
