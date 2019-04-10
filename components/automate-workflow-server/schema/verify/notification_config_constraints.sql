-- Verify delivery:notification_config_constraints on pg

BEGIN;

  SELECT id,
         notification_type,
         name,
         settings,
         enabled,
         organization_id
    FROM notification_config
   WHERE FALSE;

  SELECT 1/COUNT(*)
    FROM pg_class c
    JOIN pg_namespace n ON n.oid = c.relnamespace
   WHERE c.relname = 'notification_config_type_organization_key';

ROLLBACK;
