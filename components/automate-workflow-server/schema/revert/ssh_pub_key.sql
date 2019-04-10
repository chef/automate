-- Revert ssh_pub_key

BEGIN;

DROP DOMAIN ssh_pub_key;

COMMIT;
