--create table migration_type
CREATE TABLE IF NOT EXISTS migration_type (
  id                        int PRIMARY KEY,
  type                      TEXT NOT NULL DEFAULT ''
);

--create table migration_status
CREATE TABLE IF NOT EXISTS migration_status (
  id                        int PRIMARY KEY,
  status_message            TEXT NOT NULL DEFAULT 'In Progress'
);

--create table migration
CREATE TABLE IF NOT EXISTS migration (
  id                               SERIAL PRIMARY KEY,
  migration_id                     TEXT NOT NULL,
  server_id                        TEXT NOT NULL references servers(id) ON DELETE RESTRICT,
  type_id                          int NOT NULL references migration_type(id) ON DELETE RESTRICT,
  status_id                        int NOT NULL references migration_status(id) ON DELETE RESTRICT,
  total_succeeded                  int NOT NULL DEFAULT 0,
  total_skipped                    int NOT NULL DEFAULT 0,
  total_failed                     int NOT NULL DEFAULT 0,
  message                          TEXT NOT NULL DEFAULT '',
  updated_timestamp                TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE INDEX IF NOT EXISTS migration_server_id_index ON migration (server_id);
CREATE INDEX IF NOT EXISTS migration_migration_id_index ON migration (migration_id);

-- Insert rows into migration_type
INSERT INTO migration_type VALUES
      (100,'Migration started'),
      (1000,'Upload of zip file'),
      (1001,'Unzip of zip file'),
      (1002,'Parsing of orgs file'),
      (1003,'Parsing of users file'),
      (1004,'Parsing of user association file'),
      (1005,'Parsing of user permissions file'),
      (1006,'Creating Preview'),
      (1007,'Migration of organization'),
      (1008,'Migration of users'),
      (1009,'Association of users to orgs'),
      (1010,'Migrating user permissions'),
      (5000,'Migration Completed'),
      (6000,'Migration Cancelled')
;

-- Insert rows into migration_status
INSERT INTO migration_status VALUES
        (100,'In Progress'),
        (101,'Completed'),
        (102,'Failed')
;

-- Created type migration_records
CREATE TYPE migration_records AS (
  id                               TEXT,
  migration_id                     TEXT,
  server_id                        TEXT,
  type_id                          int,
  status_id                        int,
  total_succeeded                  int,
  total_skipped                    int,
  total_failed                     int,
  message                          TEXT,
  updated_timestamp                TIMESTAMPTZ
);

-- Insert entry into migration table
CREATE OR REPLACE FUNCTION insert_migration(migrationId TEXT, serverId TEXT, typeId int, statusId int, totalSucceeded int, totalSkipped int, totalFailed int, msg TEXT) 
RETURNS json AS
$$
DECLARE result_record migration_records;
BEGIN
	INSERT INTO migration (
		migration_id, server_id, 
		type_id, status_id, 
		total_succeeded, total_skipped, total_failed,
		message, updated_timestamp)
	VALUES (migrationId, serverId, typeId, statusId, totalSucceeded, totalSkipped, totalFailed, msg, now())
  RETURNING id, migration_id, server_id,type_id, status_id, total_succeeded, total_skipped, total_failed, message, updated_timestamp
  INTO 
    result_record.id,result_record.migration_id,result_record.server_id,
    result_record.type_id,result_record.status_id,
    result_record.total_succeeded,result_record.total_skipped,result_record.total_failed,
    result_record.message,result_record.updated_timestamp;

  RETURN row_to_json(result_record);

END;
$$
LANGUAGE plpgsql;

-- Create table migration_stage
CREATE TABLE IF NOT EXISTS migration_stage (
  id                               SERIAL PRIMARY KEY,
  migration_id                     TEXT NOT NULL UNIQUE,
  parsed_data                      json NOT NULL DEFAULT '{}',
  created_at                       TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at                       TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- Created type migration_stage_records
CREATE TYPE migration_stage_records AS (
  id                               TEXT,
  migration_id                     TEXT,
  parsed_data                      json,
  created_at                       TIMESTAMPTZ,
  updated_at                       TIMESTAMPTZ
);

-- Insert entry into migration_stage table
CREATE OR REPLACE FUNCTION insert_migration_stage(migrationId TEXT, parsedData json) 
RETURNS json AS
$$
DECLARE result_record migration_stage_records;
BEGIN
	INSERT INTO migration_stage (
		migration_id, parsed_data, 
		created_at, updated_at)
	VALUES (migrationId, parsedData, now(), now())
  RETURNING id, migration_id, parsed_data ,created_at, updated_at
  INTO 
    result_record.id,result_record.migration_id,result_record.parsed_data,
    result_record.created_at,result_record.updated_at;

  RETURN row_to_json(result_record);

END;
$$
LANGUAGE plpgsql;

-- Get entry from migration_stage table
CREATE OR REPLACE FUNCTION get_migration_stage(migrationId TEXT) 
RETURNS json AS
$$
DECLARE result_record migration_stage_records;
BEGIN
	SELECT * FROM migration_stage where migration_id = migrationId
  INTO 
    result_record.id,result_record.migration_id,result_record.parsed_data,
    result_record.created_at,result_record.updated_at;

  RETURN row_to_json(result_record);

END;
$$
LANGUAGE plpgsql;

-- Delete entry from migration_stage table
CREATE OR REPLACE FUNCTION delete_migration_stage(migrationId TEXT) 
RETURNS json AS
$$
DECLARE result_record migration_stage_records;
BEGIN
	DELETE FROM migration_stage where migration_id = migrationId
  RETURNING id, migration_id, parsed_data ,created_at, updated_at
  INTO 
    result_record.id,result_record.migration_id,result_record.parsed_data,
    result_record.created_at,result_record.updated_at;

  RETURN row_to_json(result_record);

END;
$$
LANGUAGE plpgsql;
