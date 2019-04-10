-- Revert delivery:add_project_notification_config_constraints to pg

BEGIN;

    DELETE FROM notification_config WHERE project_id IS NOT NULL;

    DROP INDEX IF EXISTS notification_config_type_project_key;

    ALTER TABLE notification_config DROP COLUMN project_id;

COMMIT;
