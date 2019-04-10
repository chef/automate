-- Revert delivery:notification_config_support_smtp from pg

BEGIN;

ALTER TABLE IF EXISTS notification_config
            DROP COLUMN enterprise_id;

DROP INDEX IF EXISTS notification_config_type_enterprise_key;

COMMIT;
