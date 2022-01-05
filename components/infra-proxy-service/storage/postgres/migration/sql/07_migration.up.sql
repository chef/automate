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
  server_id                 TEXT NOT NULL references servers(id) ON DELETE RESTRICT,
  total_succeeded           int,
  total_skipped             int,
  total_failed              int,
  created_at                TIMESTAMPTZ NOT NULL,
  updated_at                TIMESTAMPTZ NOT NULL
);
CREATE INDEX IF NOT EXISTS migration_server_id_index ON migration (server_id);

-- Insert rows into migration_type
INSERT INTO migration_type (id,type)
VALUES (md5(RANDOM()::TEXT),'user'),
       (md5(RANDOM()::TEXT),'org');

-- Insert rows into migration_status
INSERT INTO migration_status(id,status_message)
VALUES (md5(RANDOM()::TEXT),'In Progress'),
       (md5(RANDOM()::TEXT),'Completed'),
       (md5(RANDOM()::TEXT),'Failed');
