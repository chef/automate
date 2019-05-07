ALTER TABLE IF EXISTS tags
  DROP CONSTRAINT IF EXISTS tags_key_value_key;

ALTER TABLE IF EXISTS nodes_tags
  DROP CONSTRAINT IF EXISTS  nodes_tags_node_id_tag_id_key;