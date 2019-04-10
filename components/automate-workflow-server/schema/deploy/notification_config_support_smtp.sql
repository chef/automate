-- Deploy delivery:notification_config_support_smtp to pg

BEGIN;

ALTER TABLE IF EXISTS notification_config
           ADD COLUMN enterprise_id BIGINT
           REFERENCES enterprises(id)
            ON UPDATE CASCADE
            ON DELETE CASCADE;

CREATE UNIQUE INDEX notification_config_type_enterprise_key
                 ON notification_config (notification_type, enterprise_id)
              WHERE (enterprise_id IS NOT NULL);

ALTER TABLE IF EXISTS notification_config
         ALTER COLUMN name
                 DROP NOT NULL;

COMMIT;
