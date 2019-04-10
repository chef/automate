ALTER TABLE IF EXISTS nodes
  ADD COLUMN IF NOT EXISTS source_account_id  TEXT,
  DROP CONSTRAINT IF EXISTS "nodes_source_id_key",
  DROP CONSTRAINT IF EXISTS "nodes_source_id_source_region_key",
  ADD UNIQUE (source_id, source_region, source_account_id);
