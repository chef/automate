-- Verify delivery:add_telemetry_enabled_to_users on pg

BEGIN;

SELECT telemetry_enabled
FROM users
WHERE FALSE;

ROLLBACK;
