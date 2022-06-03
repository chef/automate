-- drop table migration
DROP TABLE IF EXISTS migration;

-- drop table migration_type
DROP TABLE IF EXISTS migration_type;

-- drop table migration_status
DROP TABLE IF EXISTS migration_status;

-- drop function insert_migration
DROP FUNCTION IF EXISTS insert_migration(TEXT,TEXT,INT,INT,INT,INT,INT,TEXT);

-- drop type migration_records
DROP TYPE IF EXISTS migration_records;

-- drop table migration_stage
DROP TABLE IF EXISTS migration_stage;

-- drop function insert_migration_stage
DROP FUNCTION IF EXISTS insert_migration_stage(TEXT,json);

-- drop function get_migration_stage
DROP FUNCTION IF EXISTS get_migration_stage(TEXT);

-- drop function delete_migration_stage
DROP FUNCTION IF EXISTS delete_migration_stage(TEXT);

-- drop type migration_stage_records
DROP TYPE IF EXISTS migration_stage_records;
