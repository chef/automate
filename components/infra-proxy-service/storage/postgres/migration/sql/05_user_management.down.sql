-- drop column projects in servers table 
ALTER TABLE servers DROP COLUMN projects;

-- drop table users
DROP TABLE IF EXISTS users;
