-- Verify delivery:scanner on pg

BEGIN;

SELECT
    id,
    key,
    value
FROM tags WHERE FALSE;

SELECT
    id,
    name,
    platform,
    platform_version,
    status,
    last_contact,
    manager,
    target_config
FROM nodes WHERE FALSE;

SELECT
    node_id,
    tag_id
FROM nodes_tags WHERE FALSE;

SELECT
    id,
    type,
    status
FROM agents WHERE FALSE;

SELECT
    node_id,
    agent_id
FROM nodes_agents WHERE FALSE;

SELECT
    id,
    name,
    type,
    last_modified,
    data
FROM secrets WHERE FALSE;

SELECT
    secret_id,
    tag_id
FROM secrets_tags WHERE FALSE;

SELECT
  id,
  name,
  type,
  timeout,
  retries,
  retries_left,
  status,
  start_time,
  end_time
FROM jobs WHERE FALSE;

SELECT
    job_id,
    tag_id
FROM jobs_tags WHERE FALSE;

SELECT
    job_id,
    node_id
FROM jobs_nodes WHERE FALSE;

SELECT
    id,
    url
FROM profiles WHERE FALSE;

SELECT
    job_id,
    profile_id
FROM jobs_profiles WHERE FALSE;

ROLLBACK;
