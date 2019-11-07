CREATE TABLE IF NOT EXISTS servers (
  id          TEXT NOT NULL,
  name        TEXT NOT NULL DEFAULT '',
  fqdn        TEXT NOT NULL DEFAULT '',
  ip_address  TEXT NOT NULL DEFAULT '',
  created_at  TIMESTAMPTZ NOT NULL,
  updated_at  TIMESTAMPTZ NOT NULL,
  PRIMARY KEY(id)
);

CREATE TABLE IF NOT EXISTS orgs (
  id           TEXT NOT NULL,
  name         TEXT NOT NULL DEFAULT '',
  admin_user   TEXT NOT NULL DEFAULT '', /* Added for now but can be thing of managing mutitple keys & users */
  admin_key    TEXT NOT NULL DEFAULT '',
  server_id    TEXT NOT NULL references servers(id) ON DELETE CASCADE,
  created_at   TIMESTAMPTZ NOT NULL,
  updated_at   TIMESTAMPTZ NOT NULL,
  PRIMARY KEY (id)
);
