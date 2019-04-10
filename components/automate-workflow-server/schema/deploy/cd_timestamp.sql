-- Deploy cd_timestamp
BEGIN;

CREATE DOMAIN cd_timestamp AS TIMESTAMP WITH TIME ZONE DEFAULT NOW();
COMMENT ON DOMAIN cd_timestamp IS
  'Chef Delivery non-null timestamp with TZ';

COMMIT;
