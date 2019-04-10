-- Deploy delivery:notification_config to pg

BEGIN;

CREATE TABLE IF NOT EXISTS notification_config (
    id BIGSERIAL PRIMARY KEY,
    notification_type text NOT NULL,
    name text NOT NULL,
    settings json NOT NULL,
    enabled boolean NOT NULL,
    organization_id BIGINT NOT NULL REFERENCES organizations(id) ON UPDATE CASCADE ON DELETE CASCADE,
    project_id BIGINT REFERENCES projects(id) ON UPDATE CASCADE ON DELETE CASCADE
);

COMMIT;
