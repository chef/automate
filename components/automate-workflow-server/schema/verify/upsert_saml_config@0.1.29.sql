-- Verify delivery:upsert_saml_config on pg

BEGIN;

SELECT has_function_privilege(
  'upsert_saml_config(text, text, text, text, text, text, text, text)',
  'execute');

ROLLBACK;
