ALTER TABLE IF EXISTS nodes
  ADD COLUMN IF NOT EXISTS projects_data JSON not null default '{}'::json;