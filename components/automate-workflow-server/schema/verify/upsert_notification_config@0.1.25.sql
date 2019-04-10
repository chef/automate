-- Verify delivery:upsert_notification_config on pg

BEGIN;

SELECT has_function_privilege(
  'upsert_notification_config(text, text, json, boolean, bigint)',
  'execute');

ROLLBACK;