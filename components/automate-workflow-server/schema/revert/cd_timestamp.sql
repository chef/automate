-- Revert cd_timestamp

BEGIN;

DROP DOMAIN cd_timestamp;

COMMIT;
