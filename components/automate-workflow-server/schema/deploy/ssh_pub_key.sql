-- Deploy ssh_pub_key

BEGIN;

CREATE DOMAIN ssh_pub_key AS TEXT;
COMMENT ON DOMAIN ssh_pub_key IS
  'An SSH Public Key';
COMMIT;
