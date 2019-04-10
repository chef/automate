CREATE TABLE IF NOT EXISTS tags (
  id    text NOT NULL,
  key   text NOT NULL,
  value text NOT NULL DEFAULT '',
  primary key(id)
);

CREATE TABLE IF NOT EXISTS nodes (
  id               text NOT NULL,
  name             text NOT NULL DEFAULT '',
  platform         text NOT NULL DEFAULT '',
  platform_version text NOT NULL DEFAULT '',
  status           text NOT NULL DEFAULT 'unknown',
  last_contact     timestamp DEFAULT '0001-01-01T00:00:00Z00:00',
  manager          text NOT NULL DEFAULT '',
  target_config    json,
  primary key(id)
);

CREATE TABLE IF NOT EXISTS nodes_tags (
  node_id text NOT NULL references nodes(id) ON DELETE CASCADE,
  tag_id  text NOT NULL references tags(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS agents (
  id     text NOT NULL,
  type   text NOT NULL DEFAULT '',
  status text NOT NULL DEFAULT '',
  primary key(id)
);

CREATE TABLE IF NOT EXISTS nodes_agents (
  node_id  text NOT NULL references nodes(id) ON DELETE CASCADE,
  agent_id text NOT NULL references agents(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS secrets (
  id            text      NOT NULL,
  name          text      NOT NULL DEFAULT '',
  type          text      NOT NULL DEFAULT '',
  last_modified timestamp NOT NULL DEFAULT NOW(),
  data          json      NOT NULL,
  primary key(id)
);

CREATE TABLE IF NOT EXISTS secrets_tags (
  secret_id text NOT NULL references secrets(id) ON DELETE CASCADE,
  tag_id    text NOT NULL references tags(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS jobs (
  id           text NOT NULL,
  name         text NOT NULL DEFAULT '',
  type         text NOT NULL DEFAULT '',
  timeout      int,
  retries      int,
  retries_left int,
  status       text DEFAULT 'unknown',
  start_time   timestamp DEFAULT '0001-01-01T00:00:00Z00:00',
  end_time     timestamp DEFAULT '0001-01-01T00:00:00Z00:00',
  primary key(id)
);

CREATE TABLE IF NOT EXISTS jobs_tags (
  job_id text NOT NULL references jobs(id) ON DELETE CASCADE,
  tag_id text NOT NULL references tags(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS jobs_nodes (
  job_id  text NOT NULL references jobs(id) ON DELETE CASCADE,
  node_id text NOT NULL references nodes(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS profiles (
  id   text NOT NULL,
  url  text NOT NULL DEFAULT '',
  primary key(id)
);

CREATE TABLE IF NOT EXISTS jobs_profiles (
  job_id  text NOT NULL references jobs(id) ON DELETE CASCADE,
  profile_id text NOT NULL references profiles(id) ON DELETE CASCADE
);