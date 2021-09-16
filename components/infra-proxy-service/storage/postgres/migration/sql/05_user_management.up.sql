-- add projects column
ALTER TABLE servers ADD COLUMN projects TEXT[] NOT NULL DEFAULT '{}';

-- create table users
CREATE TABLE IF NOT EXISTS users (
  id                        TEXT PRIMARY KEY,
  server_id                 TEXT NOT NULL references servers(id) ON DELETE RESTRICT,
  infra_server_username     TEXT NOT NULL DEFAULT '',
  credential_id             TEXT NOT NULL DEFAULT '',
  connector                 TEXT NOT NULL DEFAULT 'local',
  automate_user_id          TEXT NOT NULL DEFAULT '',
  is_server_admin           BOOLEAN NOT NULL DEFAULT FALSE,
  created_at                TIMESTAMPTZ NOT NULL,
  updated_at                TIMESTAMPTZ NOT NULL,
  CONSTRAINT infra_server_username_server_id_key
  UNIQUE(infra_server_username,server_id)
);
CREATE INDEX IF NOT EXISTS users_automate_user_id_index ON users (automate_user_id);
