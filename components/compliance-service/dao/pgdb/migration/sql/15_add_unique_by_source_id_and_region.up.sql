ALTER TABLE IF EXISTS nodes
  ADD UNIQUE (source_id, source_region);
