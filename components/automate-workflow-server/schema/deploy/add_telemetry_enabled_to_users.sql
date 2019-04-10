-- Deploy delivery:add_telemetry_enabled_to_users to pg

BEGIN;

ALTER TABLE users ADD COLUMN telemetry_enabled BOOLEAN;

COMMIT;
