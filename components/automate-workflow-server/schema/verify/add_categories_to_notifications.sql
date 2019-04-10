-- Verify delivery:add_categories_to_notifications on pg

BEGIN;

  SELECT categories
    FROM notification_subscriptions
   WHERE FALSE;

ROLLBACK;
