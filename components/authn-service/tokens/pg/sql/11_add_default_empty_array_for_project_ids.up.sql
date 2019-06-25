-- we default to [] instead of null
ALTER TABLE chef_authn_tokens ALTER COLUMN project_ids SET DEFAULT '{}';
