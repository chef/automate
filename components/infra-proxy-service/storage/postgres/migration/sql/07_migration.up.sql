--create table migration_type
CREATE TABLE IF NOT EXISTS migration_type (
  id                        TEXT PRIMARY KEY,
  type                      TEXT NOT NULL DEFAULT ''
);

--create table migration_status
CREATE TABLE IF NOT EXISTS migration_status (
  id                        TEXT PRIMARY KEY,
  status_message            TEXT NOT NULL DEFAULT 'In Progress'
);

--create table migration
CREATE TABLE IF NOT EXISTS migration (
  id                        TEXT PRIMARY KEY,
  type_id                   TEXT NOT NULL references migration_type(id) ON DELETE RESTRICT,
  status_id                 TEXT NOT NULL references migration_status(id) ON DELETE RESTRICT,
  total_succeeded           int,
  total_skipped             int,
  total_failed              int,
  created_at                TIMESTAMPTZ NOT NULL,
  updated_at                TIMESTAMPTZ NOT NULL
);
