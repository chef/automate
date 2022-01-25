package postgres

import (
	"context"
	"encoding/json"
	"errors"

	"github.com/chef/automate/components/infra-proxy-service/constants"
	"github.com/chef/automate/components/infra-proxy-service/storage"
)

// StartMigration Insert a migration entry to the DB for migration started
func (p *postgres) StartMigration(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.StartMigration), int64(constants.InProgress), 0, 0, 0)
}

// StartFileUpload Insert a migration entry to the DB for File upload started
func (p *postgres) StartFileUpload(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.StartFileUpload), int64(constants.InProgress), 0, 0, 0)
}

// CompleteFileUpload Insert a migration entry to the DB for File upload completed
func (p *postgres) CompleteFileUpload(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.CompleteFileUpload), int64(constants.Completed), totalSucceeded, totalSkipped, totalFailed)
}

// CompleteFileUpload Insert a migration entry to the DB for file upload failed
func (p *postgres) FailedFileUpload(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, message, int64(constants.FailedFileUpload), int64(constants.Failed), totalSucceeded, totalSkipped, totalFailed)
}

// StartUnzip Insert a migration entry to the DB for File unzip started
func (p *postgres) StartUnzip(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.StartUnzip), int64(constants.InProgress), 0, 0, 0)
}

// ComplteUnzip Insert a migration entry to the DB for File unzip completed
func (p *postgres) ComplteUnzip(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.ComplteUnzip), int64(constants.Completed), totalSucceeded, totalSkipped, totalFailed)
}

// FailedUnzip Insert a migration entry to the DB for file unzip failed
func (p *postgres) FailedUnzip(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, message, int64(constants.FailedUnzip), int64(constants.Failed), totalSucceeded, totalSkipped, totalFailed)
}

// StartZipParsing Insert a migration entry to the DB for zip parsing started
func (p *postgres) StartZipParsing(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.StartZipParsing), int64(constants.InProgress), 0, 0, 0)
}

// CompleteZipParsing Insert a migration entry to the DB for zip parsing completed
func (p *postgres) CompleteZipParsing(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.CompleteZipParsing), int64(constants.Completed), totalSucceeded, totalSkipped, totalFailed)
}

// FailedZipParsing Insert a migration entry to the DB for zip parsing failed
func (p *postgres) FailedZipParsing(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, message, int64(constants.FailedZipParsing), int64(constants.Failed), totalSucceeded, totalSkipped, totalFailed)
}

// StartOrgMigration Insert a migration entry to the DB for org migration started
func (p *postgres) StartOrgMigration(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.StartOrgMigration), int64(constants.InProgress), 0, 0, 0)
}

// CompleteOrgMigration Insert a migration entry to the DB for org migration completed
func (p *postgres) CompleteOrgMigration(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.CompleteOrgMigration), int64(constants.Completed), totalSucceeded, totalSkipped, totalFailed)
}

// FailedOrgMigration Insert a migration entry to the DB for org migration failed
func (p *postgres) FailedOrgMigration(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, message, int64(constants.FailedOrgMigration), int64(constants.Failed), totalSucceeded, totalSkipped, totalFailed)
}

// StartUserMigration Insert a migration entry to the DB for user migration started
func (p *postgres) StartUserMigration(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.StartUserMigration), int64(constants.InProgress), 0, 0, 0)
}

// CompleteUserMigration Insert a migration entry to the DB for user migration completed
func (p *postgres) CompleteUserMigration(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.CompleteUserMigration), int64(constants.Completed), totalSucceeded, totalSkipped, totalFailed)
}

// FailedUserMigration Insert a migration entry to the DB for user migration failed
func (p *postgres) FailedUserMigration(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, message, int64(constants.FailedUserMigration), int64(constants.Failed), totalSucceeded, totalSkipped, totalFailed)
}

// StartAssciation Insert a migration entry to the DB for Association of users to orgs started
func (p *postgres) StartAssciation(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.StartAssciation), int64(constants.InProgress), 0, 0, 0)
}

// CompleteAssciation Insert a migration entry to the DB for Association of users to orgs completed
func (p *postgres) CompleteAssciation(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.CompleteAssciation), int64(constants.Completed), totalSucceeded, totalSkipped, totalFailed)
}

// FailedAssciation Insert a migration entry to the DB for Association of users to orgs failed
func (p *postgres) FailedAssciation(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, message, int64(constants.FailedAssciation), int64(constants.Failed), totalSucceeded, totalSkipped, totalFailed)
}

// StartPermissionMigration Insert a migration entry to the DB for migrating user permissions started
func (p *postgres) StartPermissionMigration(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.StartPermissionMigration), int64(constants.InProgress), 0, 0, 0)
}

// CompletePermissionMigration Insert a migration entry to the DB for migrating user permissions completed
func (p *postgres) CompletePermissionMigration(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.CompletePermissionMigration), int64(constants.Completed), totalSucceeded, totalSkipped, totalFailed)
}

// FailedPermissionMigration Insert a migration entry to the DB for migrating user permissions failed
func (p *postgres) FailedPermissionMigration(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, message, int64(constants.FailedPermissionMigration), int64(constants.Failed), totalSucceeded, totalSkipped, totalFailed)
}

// CompleteMigration Insert a migration entry to the DB for migration completed
func (p *postgres) CompleteMigration(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.CompleteMigration), int64(constants.Completed), totalSucceeded, totalSkipped, totalFailed)
}

//TODO: Optimize the function parameters using variadic
//insertMigration Inserts an entry to the DB
func (p *postgres) insertMigration(ctx context.Context, migrationId, serverId, message string, migTypeId, migStatusId, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {

	var m storage.Migration
	var mByte []byte

	query := "SELECT insert_migration($1, $2, $3, $4, $5, $6, $7, $8)"
	row := p.db.QueryRowContext(ctx, query, migrationId, serverId, migTypeId, migStatusId, totalSucceeded, totalSkipped, totalFailed, message)
	err := row.Scan(&mByte)
	if err != nil {
		return storage.Migration{}, err
	}
	err = json.Unmarshal(mByte, &m)
	if err != nil {
		return storage.Migration{}, err
	}

	return m, nil
}

//StoreMigrationStage Inserts an entry to the migration_stage
// To use this function, make sure that you should pass the searialized parsed data in []byte
func (p *postgres) StoreMigrationStage(ctx context.Context, migrationId string, parsedData interface{}) (storage.MigrationStage, error) {
	return p.insertMigrationStage(ctx, migrationId, parsedData)
}

//GetMigrationStage Get entry to the migration_stage
func (p *postgres) GetMigrationStage(ctx context.Context, migrationId string) (storage.MigrationStage, error) {
	return p.getMigrationStage(ctx, migrationId)
}

//DeleteMigrationStage Delete entry from migration_stage
func (p *postgres) DeleteMigrationStage(ctx context.Context, migrationId string) (storage.MigrationStage, error) {
	return p.deleteMigrationStage(ctx, migrationId)
}

//insertMigrationStage Inserts an entry to the migration_stage
func (p *postgres) insertMigrationStage(ctx context.Context, migrationId string, parsedData interface{}) (storage.MigrationStage, error) {

	var m storage.MigrationStage
	var mByte []byte
	var ok bool
	if mByte, ok = parsedData.([]byte); !ok {
		return m, errors.New("Cannot parse the data")
	}
	query := "SELECT insert_migration_stage($1, $2)"
	row := p.db.QueryRowContext(ctx, query, migrationId, mByte)
	err := row.Scan(&mByte)
	if err != nil {
		return storage.MigrationStage{}, err
	}
	err = json.Unmarshal(mByte, &m)
	if err != nil {
		return storage.MigrationStage{}, err
	}
	return m, nil
}

//getMigrationStage Get an entry from migration_stage
func (p *postgres) getMigrationStage(ctx context.Context, migrationId string) (storage.MigrationStage, error) {

	var m storage.MigrationStage
	var mByte []byte

	query := "SELECT get_migration_stage($1)"
	row := p.db.QueryRowContext(ctx, query, migrationId)
	err := row.Scan(&mByte)
	if err != nil {
		return storage.MigrationStage{}, err
	}
	err = json.Unmarshal(mByte, &m)
	if err != nil {
		return storage.MigrationStage{}, err
	}
	return m, nil
}

//deleteMigrationStage Delete an entry from migration_stage
func (p *postgres) deleteMigrationStage(ctx context.Context, migrationId string) (storage.MigrationStage, error) {

	var m storage.MigrationStage
	var mByte []byte

	query := "SELECT delete_migration_stage($1)"
	row := p.db.QueryRowContext(ctx, query, migrationId)
	err := row.Scan(&mByte)
	if err != nil {
		return storage.MigrationStage{}, err
	}
	err = json.Unmarshal(mByte, &m)
	if err != nil {
		return storage.MigrationStage{}, err
	}
	return m, nil
}
