-- ALTER TABLE IF EXISTS tags
--   ADD UNIQUE (key, value);

-- ALTER TABLE IF EXISTS nodes_tags
--   ADD UNIQUE (node_id, tag_id);

-- Setting a unique on these tables doesn't work for instances
-- that already have a lot of data in them, because some duplicates 
-- already exist, so we decided to remove this change and instead revert
-- back to doing the dup checking in code