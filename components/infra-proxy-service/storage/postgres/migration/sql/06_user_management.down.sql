-- drop column projects in servers table 
ALTER TABLE servers DROP COLUMN projects;
ALTER TABLE servers DROP COLUMN credential_id;

-- drop table org_users
DROP TABLE IF EXISTS org_users;

-- drop table users
DROP TABLE IF EXISTS users;
