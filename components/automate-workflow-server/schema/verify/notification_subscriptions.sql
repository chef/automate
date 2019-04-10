-- Verify delivery:notification_subscriptions on pg

BEGIN;

  SELECT id,
         project_id,
         user_id
    FROM notification_subscriptions
   WHERE FALSE;

   SELECT 1/COUNT(*)
     FROM pg_class c
     JOIN pg_namespace n ON n.oid = c.relnamespace
    WHERE c.relname = 'notification_subscriptions_project_id_user_id_key';

ROLLBACK;
