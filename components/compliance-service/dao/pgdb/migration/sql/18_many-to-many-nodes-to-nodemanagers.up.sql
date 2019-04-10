CREATE TABLE IF NOT EXISTS node_managers_nodes (
  manager_id text NOT NULL REFERENCES node_managers(id) ON DELETE CASCADE,
  node_id text NOT NULL REFERENCES nodes(id) ON DELETE CASCADE,
  PRIMARY KEY(manager_id, node_id)
);

INSERT INTO node_managers_nodes (manager_id, node_id)
  SELECT manager_id, id from nodes WHERE manager_id IS NOT NULL
  ON CONFLICT DO NOTHING;

ALTER TABLE IF EXISTS nodes DROP COLUMN IF EXISTS manager_id;
