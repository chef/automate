CREATE TABLE sessions (
  token TEXT PRIMARY KEY,
  data BYTEA NOT NULL,
  expiry TIMESTAMPTZ NOT NULL
);
CREATE INDEX sessions_expiry_idx ON sessions (expiry);

CREATE TABLE blacklisted_id_tokens (
    id_token TEXT
);
CREATE INDEX blacklist_session_idx ON blacklisted_id_tokens (id_token);

