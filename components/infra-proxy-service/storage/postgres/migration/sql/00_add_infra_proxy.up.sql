CREATE TABLE IF NOT EXISTS servers (
  id          uuid PRIMARY KEY,
  name        TEXT NOT NULL UNIQUE,
  description TEXT NOT NULL DEFAULT '',
  fqdn        TEXT NOT NULL DEFAULT '',
  ip_address  TEXT NOT NULL DEFAULT '',
  created_at  TIMESTAMPTZ NOT NULL,
  updated_at  TIMESTAMPTZ NOT NULL
);

CREATE TABLE IF NOT EXISTS orgs (
  id           uuid PRIMARY KEY,
  name         TEXT NOT NULL DEFAULT '',
  admin_user   TEXT NOT NULL DEFAULT '', /* Added for now but can be thing of managing mutitple keys & users */
  admin_key    TEXT NOT NULL DEFAULT '',
  server_id    uuid NOT NULL references servers(id) ON DELETE CASCADE,
  created_at   TIMESTAMPTZ NOT NULL,
  updated_at   TIMESTAMPTZ NOT NULL,
  UNIQUE(name, server_id)
);
