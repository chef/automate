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
  id                               TEXT PRIMARY KEY,
  migration_id                     TEXT NOT NULL DEFAULT '',
  server_id                        TEXT NOT NULL references servers(id) ON DELETE RESTRICT,
  type_id                          int NOT NULL references migration_type(id) ON DELETE RESTRICT,
  status_id                        int NOT NULL references migration_status(id) ON DELETE RESTRICT,
  total_succeeded                  int NOT NULL DEFAULT 0,
  total_skipped                    int NOT NULL DEFAULT 0,
  total_failed                     int NOT NULL DEFAULT 0,
  updated_timestamp                TIMESTAMPTZ NOT NULL
);
CREATE INDEX IF NOT EXISTS migration_server_id_index ON migration (server_id);

-- Insert rows into migration_type
INSERT INTO migration_type VALUES
       (1,'Parsing of zip file started'),
       (2,'Parsing of zip file completed'),
       (3,'Migration of organization started'),
       (4,'Migration of organization completed'),
       (5,'Migration of users started'),
       (6,'Migration of users completed'),
       (7,'Association of users to orgs started'),
       (8,'Association of users to orgs completed'),
       (9,'Migrating user permissions started'),
       (10,'Migrating user permissions completed')
;

-- Insert rows into migration_status
INSERT INTO migration_status VALUES
        (1,'In Progress'),
        (2,'Completed'),
        (3,'  Failed')
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
  updated_timestamp                TIMESTAMPTZ
);

-- Insert entry into migration table
CREATE OR REPLACE FUNCTION insert_migration(_id TEXT, migrationId TEXT, serverId TEXT, typeId int, statusId int, totalSucceeded int, totalSkipped int, totalFailed int) 
RETURNS json AS
$$
DECLARE result_record migration_records;
BEGIN
	INSERT INTO migration (
		id, migration_id, server_id, 
		type_id, status_id, 
		total_succeeded, total_skipped, total_failed,
		updated_timestamp)
	VALUES (_id, migrationId, serverId, typeId, statusId, totalSucceeded, totalSkipped, totalFailed, now())
  RETURNING id, migration_id, server_id,type_id, status_id, total_succeeded, total_skipped, total_failed, updated_timestamp
  INTO 
    result_record.id,result_record.migration_id,result_record.server_id,
    result_record.type_id,result_record.status_id,
    result_record.total_succeeded,result_record.total_skipped,result_record.total_failed,result_record.updated_timestamp;

  RETURN row_to_json(result_record);

END;
$$
LANGUAGE plpgsql;

