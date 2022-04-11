-- add projects column
ALTER TABLE servers ADD COLUMN projects TEXT[] NOT NULL DEFAULT '{}';
ALTER TABLE servers ADD COLUMN credential_id TEXT NOT NULL DEFAULT '';

-- create table users
CREATE TABLE IF NOT EXISTS users (
  id                        SERIAL PRIMARY KEY,
  server_id                 TEXT NOT NULL references servers(id) ON DELETE CASCADE,
  infra_server_username     TEXT NOT NULL DEFAULT '',
  connector                 TEXT NOT NULL DEFAULT 'local',
  automate_user_id          TEXT NOT NULL DEFAULT '',
  first_name                TEXT NOT NULL DEFAULT '',
  last_name                 TEXT NOT NULL DEFAULT '',
  email                     TEXT NOT NULL DEFAULT '',
  middle_name               TEXT NOT NULL DEFAULT '',
  display_name              TEXT NOT NULL DEFAULT '',
  created_at                TIMESTAMPTZ NOT NULL,
  updated_at                TIMESTAMPTZ NOT NULL,
  CONSTRAINT infra_server_username_server_id_key
  UNIQUE(infra_server_username,server_id)
);
CREATE INDEX IF NOT EXISTS users_automate_user_id_index ON users (automate_user_id);

-- create table org_users
CREATE TABLE IF NOT EXISTS org_users (
  id                        SERIAL PRIMARY KEY,
  org_id                    TEXT NOT NULL,
  user_id                   INTEGER NOT NULL references users(id) ON DELETE CASCADE, 
  is_admin                  BOOLEAN NOT NULL DEFAULT FALSE,
  created_at                TIMESTAMPTZ NOT NULL,
  updated_at                TIMESTAMPTZ NOT NULL
);
CREATE INDEX IF NOT EXISTS org_users_org_id_index ON org_users (org_id);
CREATE INDEX IF NOT EXISTS org_users_user_id_index ON org_users (user_id);
