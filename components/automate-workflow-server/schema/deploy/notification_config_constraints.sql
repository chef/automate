-- Deploy delivery:notification_config_constraints to pg

BEGIN;

    ALTER TABLE notification_config DROP COLUMN project_id;

    ALTER TABLE notification_config ALTER COLUMN organization_id DROP NOT NULL;

    CREATE UNIQUE INDEX notification_config_type_organization_key
        ON notification_config (notification_type, organization_id)
     WHERE (organization_id IS NOT NULL);

COMMIT;
