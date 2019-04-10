-- Deploy delivery:set_users_telemetry_enabled_to_null to pg

BEGIN;

  UPDATE users SET telemetry_enabled=NULL WHERE telemetry_enabled=FALSE;

COMMIT;
