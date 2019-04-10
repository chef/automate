ALTER TABLE IF EXISTS nodes
  DROP CONSTRAINT IF EXISTS "nodes_source_id_source_region_source_account_id_key",
  DROP COLUMN IF EXISTS source_account_id,
  ADD UNIQUE (source_id, source_region);
