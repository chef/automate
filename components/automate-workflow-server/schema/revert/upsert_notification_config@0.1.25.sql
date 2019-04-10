-- Revert delivery:upsert_notification_config from pg

BEGIN;

DROP FUNCTION IF EXISTS upsert_notification_config(
    p_notification_type notification_config.notification_type%TYPE,
    p_name notification_config.name%TYPE,
    p_settings notification_config.settings%TYPE,
    p_enabled notification_config.enabled%TYPE,
    p_organization_id notification_config.organization_id%TYPE
);

COMMIT;
