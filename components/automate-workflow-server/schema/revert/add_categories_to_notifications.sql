-- Revert delivery:add_categories_to_notifications from pg

BEGIN;

  ALTER TABLE IF EXISTS notification_subscriptions DROP COLUMN categories;

COMMIT;
