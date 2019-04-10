CREATE TABLE IF NOT EXISTS tags (
  id    text NOT NULL,
  key   text NOT NULL,
  value text NOT NULL DEFAULT '',
  primary key(id)
);

CREATE TABLE IF NOT EXISTS nodes (
  id               text NOT NULL, /* exists in a1 */
  name             text NOT NULL DEFAULT '', /* exists in a1 */
  platform         text NOT NULL DEFAULT '', /* exists in a1 */
  platform_version text NOT NULL DEFAULT '', /* exists in a1 */
  status           text NOT NULL DEFAULT 'unknown', /* exists in a1 */
  last_contact     timestamp DEFAULT '0001-01-01T00:00:00Z00:00', /* exists in a1 */
  manager          text NOT NULL DEFAULT '', /* exists in a1 */
  target_config    json, /* exists in a1 */
  last_job         text NOT NULL DEFAULT '', /* exists in a1 */
  primary key(id)
);

/* tables migrated from a1 will not have certain fields, 
so we explicitly add them here
instead of just including in the table definition above */
ALTER TABLE IF EXISTS nodes
  ADD COLUMN IF NOT EXISTS instance_credentials JSON NOT NULL DEFAULT '[]',
  ADD COLUMN IF NOT EXISTS source_id TEXT DEFAULT NULL,
  ADD COLUMN IF NOT EXISTS date_added TIMESTAMP NOT NULL DEFAULT NOW(),
  ADD COLUMN IF NOT EXISTS last_connection_attempt timestamp DEFAULT '0001-01-01T00:00:00Z00:00',
  ADD COLUMN IF NOT EXISTS source_region TEXT DEFAULT '',
  ADD COLUMN IF NOT EXISTS source_state TEXT DEFAULT '',
  ADD COLUMN IF NOT EXISTS source_account_id TEXT DEFAULT '',
  ADD COLUMN IF NOT EXISTS connection_error text NOT NULL DEFAULT '',
  ADD COLUMN IF NOT EXISTS statechange_timestamp timestamp NOT NULL DEFAULT '0001-01-01T00:00:00Z00:00',
  ADD COLUMN IF NOT EXISTS report_id text DEFAULT '';


CREATE TABLE IF NOT EXISTS nodes_secrets (
  node_id   text NOT NULL references nodes(id) ON DELETE CASCADE,
  secret_id text NOT NULL DEFAULT ''
);

ALTER TABLE IF EXISTS nodes
  ADD UNIQUE (source_id, source_region, source_account_id);

CREATE TABLE IF NOT EXISTS nodes_tags (
  node_id text NOT NULL references nodes(id) ON DELETE CASCADE,
  tag_id  text NOT NULL references tags(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS node_managers (
  id             TEXT NOT NULL, /* exists in a1 */
  name           TEXT NOT NULL DEFAULT '', /* exists in a1 */
  type           TEXT NOT NULL DEFAULT '', /* exists in a1 */
  credentials    TEXT NOT NULL DEFAULT '', /* exists in a1 */
  status_type    TEXT NOT NULL DEFAULT '', /* exists in a1 */
  status_message TEXT NOT NULL DEFAULT '', /* exists in a1 */
  PRIMARY KEY (id)
);

/* tables migrated from a1 will not have certain fields, 
so we explicitly add them here
instead of just including in the table definition above */
ALTER TABLE IF EXISTS node_managers
  ADD COLUMN IF NOT EXISTS account_id TEXT,
  ADD COLUMN IF NOT EXISTS instance_credentials JSON NOT NULL DEFAULT '[]',
  ADD COLUMN IF NOT EXISTS date_added timestamp DEFAULT '0001-01-01T00:00:00Z00:00',
  ADD UNIQUE (credentials, type);

CREATE TABLE IF NOT EXISTS node_managers_nodes (
  manager_id text NOT NULL REFERENCES node_managers(id) ON DELETE CASCADE,
  node_id text NOT NULL REFERENCES nodes(id) ON DELETE CASCADE,
  PRIMARY KEY(manager_id, node_id)
);

CREATE TABLE IF NOT EXISTS projects (
  id          text NOT NULL,
  project_id  text NOT NULL UNIQUE,
  primary key(id),
  CHECK(length(project_id) > 0)
);

CREATE TABLE IF NOT EXISTS nodes_projects (
  node_id     text NOT NULL,
  project_id  text NOT NULL,
  CHECK(length(node_id) > 0),
  CHECK(length(project_id) > 0),
  UNIQUE(node_id, project_id)
);