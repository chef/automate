-- Revert delivery:notification_subscriptions from pg

BEGIN;

  DROP INDEX IF EXISTS notification_subscriptions_project_id_user_id_key;

  DROP TABLE notification_subscriptions;

COMMIT;
