ALTER TABLE IF EXISTS node_managers
  ADD COLUMN IF NOT EXISTS instance_credentials JSON NOT NULL DEFAULT '[]';
