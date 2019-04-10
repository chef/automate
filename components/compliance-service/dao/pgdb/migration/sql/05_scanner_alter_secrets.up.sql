ALTER TABLE IF EXISTS nodes_secrets
  DROP CONSTRAINT IF EXISTS nodes_secrets_secret_id_fkey;

CREATE TABLE IF NOT EXISTS s_tags (
  id    TEXT NOT NULL,
  key   TEXT NOT NULL,
  value TEXT NOT NULL DEFAULT '',
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS s_secrets (
  id            TEXT      NOT NULL,
  name          TEXT      NOT NULL DEFAULT '',
  type          TEXT      NOT NULL DEFAULT '',
  last_modified TIMESTAMP NOT NULL DEFAULT NOW(),
  data          TEXT      NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS s_secrets_tags (
  secret_id TEXT NOT NULL REFERENCES s_secrets (id) ON DELETE CASCADE,
  tag_id    TEXT NOT NULL REFERENCES s_tags (id) ON DELETE CASCADE
);

DROP TABLE IF EXISTS secrets_tags;
DROP TABLE IF EXISTS secrets;
