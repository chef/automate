-- Deploy delivery:oauth_tokens to pg
-- requires: external_oauth_applications

BEGIN;

CREATE TABLE IF NOT EXISTS oauth_tokens(
  id BIGSERIAL PRIMARY KEY,
  oauth_app_id BIGINT REFERENCES external_oauth_applications(id) ON DELETE CASCADE,
  scope delivery_scope,
  scope_id BIGINT,
  state TEXT UNIQUE,
  token TEXT,
  UNIQUE(oauth_app_id, scope, scope_id)
);

COMMENT ON TABLE oauth_tokens IS
$$
Holds the OAuth tokens for the various OAuth integrations.
$$;

COMMENT ON COLUMN oauth_tokens.oauth_app_id IS
$$
Foreign key to the OAuth app from external_oauth_applications
$$;

COMMENT ON COLUMN oauth_tokens.scope IS
$$
The scope for which this token has privileges in our system.
$$;

COMMENT ON COLUMN oauth_tokens.scope_id IS
$$
The specific instance of the associated scope for which this token is valid (i.e.
  the project ID if the scope was project).
$$;

COMMENT ON COLUMN oauth_tokens.state IS
$$
The state generated as part of the OAuth handshake.
$$;

COMMENT ON COLUMN oauth_tokens.token IS
$$
The OAuth token
$$;


COMMIT;
