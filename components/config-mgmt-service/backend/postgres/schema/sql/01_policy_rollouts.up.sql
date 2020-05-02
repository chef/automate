CREATE TABLE IF NOT EXISTS policy_rollout (
  id          SERIAL PRIMARY KEY,
  started_at  TIMESTAMP NOT NULL DEFAULT NOW(),
  policy_content_server_url TEXT NOT NULL DEFAULT 'default',
  policy_node_group TEXT NOT NULL,
  policy_name TEXT NOT NULL
  -- git_commit_id TEXT,
  -- description TEXT NOT NULL DEFAULT '',
  -- initiated_by_username TEXT,
  -- initiated_by_url TEXT
);

CREATE INDEX policy_rollout_started_at ON policy_rollout (started_at);
CREATE INDEX policy_rollout_policy_content_server_url ON policy_rollout (policy_content_server_url);
CREATE INDEX policy_rollout_policy_node_group ON policy_rollout (policy_node_group);
CREATE INDEX policy_rollout_policy_name ON policy_rollout (policy_name);
