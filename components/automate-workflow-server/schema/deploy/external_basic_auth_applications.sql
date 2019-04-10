-- Deploy delivery:external_basic_auth_applications to pg

BEGIN;

CREATE TABLE IF NOT EXISTS external_basic_auth_applications(
  id BIGSERIAL PRIMARY KEY,
  -- The identifer you will use for this application. Must be unique in the system.
  name TEXT NOT NULL UNIQUE,
  -- The root API URL for the application. This will be used to communicate
  -- with the application's API.
  root_api_url TEXT NOT NULL,
  -- The user ID
  user_id TEXT NOT NULL,
  -- The password
  password TEXT NOT NULL
);

COMMENT ON TABLE external_basic_auth_applications IS
'Configuration used to allow Delivery to communicate with different basic auth
 protected APIs.';

COMMIT;
