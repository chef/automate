ALTER TABLE IF EXISTS tags
  ADD UNIQUE (key, value);

ALTER TABLE IF EXISTS nodes_tags
  ADD UNIQUE (node_id, tag_id);