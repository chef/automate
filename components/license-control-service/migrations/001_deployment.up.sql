BEGIN;

-- create table deployment
CREATE TABLE IF NOT EXISTS deployment (
  id          TEXT PRIMARY KEY,
  created_at  TIMESTAMPTZ NOT NULL,
  updated_at  TIMESTAMPTZ NOT NULL
);

COMMIT;
