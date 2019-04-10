CREATE TABLE IF NOT EXISTS secrets (
  id            TEXT      NOT NULL,
  name          TEXT      NOT NULL DEFAULT '',
  type          TEXT      NOT NULL DEFAULT '',
  last_modified TIMESTAMP NOT NULL DEFAULT NOW(),
  data          TEXT      NOT NULL,
  PRIMARY KEY (id)
);

INSERT INTO secrets
  SELECT *
  FROM s_secrets;

ALTER TABLE IF EXISTS nodes_secrets
  DROP CONSTRAINT IF EXISTS nodes_secrets_secret_id_fkey,
  add constraint nodes_secrets_secret_id_fkey FOREIGN KEY (secret_id) REFERENCES secrets(id);

CREATE TABLE IF NOT EXISTS secrets_tags (
  secret_id TEXT NOT NULL REFERENCES secrets (id) ON DELETE CASCADE,
  tag_id    TEXT NOT NULL REFERENCES tags (id) ON DELETE CASCADE
);

DROP TABLE IF EXISTS s_secrets_tags;
DROP TABLE IF EXISTS s_secrets;
DROP TABLE IF EXISTS s_tags;
