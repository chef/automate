-- Deploy delivery:add_projectnotification_config_constraints to pg

BEGIN;

    ALTER TABLE notification_config ADD COLUMN project_id BIGINT
      REFERENCES projects(id) ON UPDATE CASCADE ON DELETE CASCADE;

    CREATE UNIQUE INDEX notification_config_type_project_key
        ON notification_config (notification_type, project_id)
     WHERE (project_id IS NOT NULL);

COMMIT;
