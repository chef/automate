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
      (1000,'Upload of zip file started'),
      (1001,'Upload of zip file completed'),
      (1002,'Upload of zip file failed'),
      (1003,'Unzip of file started'),
      (1004,'Unzip of file completed'),
      (1005,'Unzip of file failed'),
      (1006,'Parsing of zip file started'),
      (1007,'Parsing of zip file completed'),
      (1008,'Parsing of zip file failed'),
      (1009,'Migration of organization started'),
      (1010,'Migration of organization completed'),
      (1011,'Migration of organization failed'),
      (1012,'Migration of users started'),
      (1013,'Migration of users completed'),
      (1014,'Migration of users failed'),
      (1015,'Association of users to orgs started'),
      (1016,'Association of users to orgs completed'),
      (1017,'Association of users to orgs failed'),
      (1018,'Migrating user permissions started'),
      (1019,'Migrating user permissions completed'),
      (1020,'Migrating user permissions failed'),
      (5000,'Migration Completed')
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
