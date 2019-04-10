-- Deploy delivery:external_oauth_applications to pg

BEGIN;

CREATE TABLE IF NOT EXISTS external_oauth_applications(
  id BIGSERIAL PRIMARY KEY,
  -- The identifer you will use for this application. Must be unique in the system.
  name TEXT NOT NULL UNIQUE,
  -- The name of the internal Erlang module used to facilicate API calls with
  -- this application.
  module TEXT NOT NULL,
  -- The root URL for the application. This will be used to communicate with
  -- the OAuth interfaces which are commonly scoped differently than the API.
  root_url TEXT NOT NULL,
  -- The root API URL for the application. This will be used to communicate
  -- with the application's API.
  root_api_url TEXT NOT NULL,
  -- The OAuth application ID
  client_id TEXT NOT NULL,
  -- The OAuth application secret.
  client_secret TEXT NOT NULL
);

COMMENT ON TABLE external_oauth_applications IS
'Configuration used to allow Delivery to communication with different OAuth
projected APIs.';

COMMIT;
