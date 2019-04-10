-- Deploy delivery:add_categories_to_notifications to pg
-- requires: notification_subscriptions

BEGIN;

  ALTER TABLE IF EXISTS notification_subscriptions ADD COLUMN categories TEXT[] NOT NULL;

  COMMENT ON COLUMN notification_subscriptions.categories IS
  $$
  An array of the notification categories to which a user is subscribed - for
  example: {'review', 'deliver', 'observe'}
  $$;

COMMIT;
