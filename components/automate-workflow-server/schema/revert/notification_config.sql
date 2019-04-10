-- Revert delivery:notification_config from pg

BEGIN;
 DROP TABLE IF EXISTS notification_config;
COMMIT;
