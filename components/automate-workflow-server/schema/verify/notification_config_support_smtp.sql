-- Verify delivery:notification_config_support_smtp on pg
BEGIN;

  SELECT enterprise_id
    FROM notification_config
   WHERE FALSE;

  SELECT 1/COUNT(*)
    FROM pg_class c
    JOIN pg_namespace n ON n.oid = c.relnamespace
   WHERE c.relname = 'notification_config_type_enterprise_key';
ROLLBACK;
