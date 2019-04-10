-- Verify delivery:insert_notification_subscription on pg

BEGIN;

  SELECT has_function_privilege(
    'upsert_notification_subscription(TEXT, TEXT, TEXT, TEXT, TEXT[])',
    'execute');

ROLLBACK;
