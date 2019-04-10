-- Revert delivery:add_telemetry_enabled_to_users from pg

BEGIN;

ALTER TABLE users DROP COLUMN telemetry_enabled;

COMMIT;
