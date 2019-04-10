-- Revert delivery:notification_config_constraints from pg

BEGIN;

    ALTER TABLE notification_config ADD COLUMN project_id BIGINT
        REFERENCES projects(id) ON UPDATE CASCADE ON DELETE CASCADE;

    DROP INDEX IF EXISTS notification_config_type_organization_key;

COMMIT;
