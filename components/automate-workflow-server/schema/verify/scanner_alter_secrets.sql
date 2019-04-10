-- Verify delivery:scanner_alter_secrets on pg

BEGIN;

SELECT
  id,
  name,
  type,
  last_modified,
  data
FROM s_secrets WHERE FALSE;

SELECT
  secret_id,
  tag_id
FROM s_secrets_tags WHERE FALSE;

SELECT
  id,
  key,
  value
FROM s_tags WHERE FALSE;

ROLLBACK;