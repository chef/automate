ALTER TABLE IF EXISTS nodes
  ADD COLUMN IF NOT EXISTS last_scan JSON not null default '{}'::json,
  ADD COLUMN IF NOT EXISTS last_run JSON not null default '{}'::json;
