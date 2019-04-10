-- Verify delivery:set_users_telemetry_enabled_to_null on pg

BEGIN;

  SELECT id,
       enterprise_id,
       name,
       ssh_pub_key,
       first_name,
       last_name,
       email,
       user_type,
       telemetry_enabled
    FROM users WHERE FALSE;

ROLLBACK;
