ALTER TABLE IF EXISTS nodes ADD COLUMN IF NOT EXISTS manager_id TEXT REFERENCES node_managers(id);

UPDATE nodes n SET (manager_id) = (
    SELECT manager_id
    FROM node_managers_nodes nmn
    WHERE n.id = nmn.node_id
  );

DROP TABLE node_managers_nodes IF EXISTS;
