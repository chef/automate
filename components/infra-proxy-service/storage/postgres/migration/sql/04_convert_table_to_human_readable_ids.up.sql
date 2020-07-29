-- drop foreign key constraint.
ALTER TABLE orgs DROP CONSTRAINT IF EXISTS orgs_server_id_fkey;

-- remove UUID column data type.
ALTER TABLE servers ALTER COLUMN id TYPE TEXT;
ALTER TABLE orgs ALTER COLUMN id  TYPE TEXT;
ALTER TABLE orgs ALTER COLUMN server_id TYPE TEXT;

-- add foreign key constraint.
ALTER TABLE orgs ADD CONSTRAINT orgs_server_id_fkey 
FOREIGN KEY (server_id) REFERENCES servers(id) ON DELETE RESTRICT;

-- rename uniqueness constraint.
ALTER TABLE servers DROP CONSTRAINT IF EXISTS servers_name_key;
ALTER TABLE orgs DROP CONSTRAINT IF EXISTS orgs_name_server_id_key;

-- drop description column.
ALTER TABLE servers DROP COLUMN IF EXISTS description;

-- add unique constraint.
ALTER TABLE orgs ADD CONSTRAINT orgs_name_server_id_key
UNIQUE (id, server_id);

