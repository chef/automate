CREATE TABLE IF NOT EXISTS deployment (
  id          SERIAL PRIMARY KEY,
  app_name    TEXT   NOT NULL DEFAULT '',
  environment TEXT   NOT NULL DEFAULT ''
);

CREATE TABLE IF NOT EXISTS supervisor (
  id        SERIAL PRIMARY KEY,
  member_id TEXT   NOT NULL UNIQUE,
  fqdn      TEXT   NOT NULL DEFAULT ''
);

CREATE TABLE IF NOT EXISTS service_group (
  id            SERIAL  PRIMARY KEY,
  name          TEXT    NOT NULL,
  deployment_id INTEGER NOT NULL REFERENCES deployment (id) ON DELETE CASCADE,
  UNIQUE (name, deployment_id)
);

CREATE TABLE IF NOT EXISTS service (
  id            SERIAL  PRIMARY KEY,
  origin        TEXT    NOT NULL,
  name          TEXT    NOT NULL,
  version       TEXT    NOT NULL,
  release       TEXT    NOT NULL,
  status        TEXT    NOT NULL,
  health        TEXT    NOT NULL,
  group_id      INTEGER NOT NULL REFERENCES service_group (id) ON DELETE CASCADE,
  deployment_id INTEGER NOT NULL REFERENCES deployment (id)    ON DELETE CASCADE,
  sup_id        INTEGER NOT NULL REFERENCES supervisor (id)    ON DELETE CASCADE,
  UNIQUE (sup_id, origin, name)
);

-- The network_members table associates supervisors that are members of a network,
-- that is when there are two or more supervisors talking to each other.
--
-- This table will help us distinguish between services that belong to the same service_group.
-- CREATE TABLE IF NOT EXISTS network_members (
  -- id     TEXT NOT NULL,
  -- sup_id TEXT NOT NULL REFERENCES supervisor (id) ON DELETE CASCADE
-- );
