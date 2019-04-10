-- Revert delivery:insert_notification_subscription from pg

BEGIN;

  DROP FUNCTION IF EXISTS upsert_notification_subscription(
    p_enterprise_name enterprises.name%TYPE,
    p_organization_name organizations.name%TYPE,
    p_project_name projects.name%TYPE,
    p_user_name users.name%TYPE,
    p_categories TEXT[]
  );

COMMIT;
