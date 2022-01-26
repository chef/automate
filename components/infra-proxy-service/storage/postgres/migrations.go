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
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.FileUpload), int64(constants.InProgress), 0, 0, 0)
}

// CompleteFileUpload Insert a migration entry to the DB for File upload completed
func (p *postgres) CompleteFileUpload(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.FileUpload), int64(constants.Completed), totalSucceeded, totalSkipped, totalFailed)
}

// CompleteFileUpload Insert a migration entry to the DB for file upload failed
func (p *postgres) FailedFileUpload(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, message, int64(constants.FileUpload), int64(constants.Failed), totalSucceeded, totalSkipped, totalFailed)
}

// StartUnzip Insert a migration entry to the DB for File unzip started
func (p *postgres) StartUnzip(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.FileUnzip), int64(constants.InProgress), 0, 0, 0)
}

// CompleteUnzip Insert a migration entry to the DB for File unzip completed
func (p *postgres) CompleteUnzip(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.FileUnzip), int64(constants.Completed), totalSucceeded, totalSkipped, totalFailed)
}

// FailedUnzip Insert a migration entry to the DB for file unzip failed
func (p *postgres) FailedUnzip(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, message, int64(constants.FileUnzip), int64(constants.Failed), totalSucceeded, totalSkipped, totalFailed)
}

// StartZipParsing Insert a migration entry to the DB for zip parsing started
func (p *postgres) StartZipParsing(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.ZipParsing), int64(constants.InProgress), 0, 0, 0)
}

// CompleteZipParsing Insert a migration entry to the DB for zip parsing completed
func (p *postgres) CompleteZipParsing(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.ZipParsing), int64(constants.Completed), totalSucceeded, totalSkipped, totalFailed)
}

// FailedZipParsing Insert a migration entry to the DB for zip parsing failed
func (p *postgres) FailedZipParsing(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, message, int64(constants.ZipParsing), int64(constants.Failed), totalSucceeded, totalSkipped, totalFailed)
}

// StartOrgMigration Insert a migration entry to the DB for org migration started
func (p *postgres) StartOrgMigration(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.OrgMigration), int64(constants.InProgress), 0, 0, 0)
}

// CompleteOrgMigration Insert a migration entry to the DB for org migration completed
func (p *postgres) CompleteOrgMigration(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.OrgMigration), int64(constants.Completed), totalSucceeded, totalSkipped, totalFailed)
}

// FailedOrgMigration Insert a migration entry to the DB for org migration failed
func (p *postgres) FailedOrgMigration(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, message, int64(constants.OrgMigration), int64(constants.Failed), totalSucceeded, totalSkipped, totalFailed)
}

// StartUserMigration Insert a migration entry to the DB for user migration started
func (p *postgres) StartUserMigration(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.UserMigration), int64(constants.InProgress), 0, 0, 0)
}

// CompleteUserMigration Insert a migration entry to the DB for user migration completed
func (p *postgres) CompleteUserMigration(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.UserMigration), int64(constants.Completed), totalSucceeded, totalSkipped, totalFailed)
}

// FailedUserMigration Insert a migration entry to the DB for user migration failed
func (p *postgres) FailedUserMigration(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, message, int64(constants.UserMigration), int64(constants.Failed), totalSucceeded, totalSkipped, totalFailed)
}

// StartAssociation Insert a migration entry to the DB for Association of users to orgs started
func (p *postgres) StartAssociation(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.UserAssociation), int64(constants.InProgress), 0, 0, 0)
}

// CompleteAssociation Insert a migration entry to the DB for Association of users to orgs completed
func (p *postgres) CompleteAssociation(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.UserAssociation), int64(constants.Completed), totalSucceeded, totalSkipped, totalFailed)
}

// FailedAssociation Insert a migration entry to the DB for Association of users to orgs failed
func (p *postgres) FailedAssociation(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, message, int64(constants.UserAssociation), int64(constants.Failed), totalSucceeded, totalSkipped, totalFailed)
}

// StartPermissionMigration Insert a migration entry to the DB for migrating user permissions started
func (p *postgres) StartPermissionMigration(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.PermissionMigration), int64(constants.InProgress), 0, 0, 0)
}

// CompletePermissionMigration Insert a migration entry to the DB for migrating user permissions completed
func (p *postgres) CompletePermissionMigration(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.PermissionMigration), int64(constants.Completed), totalSucceeded, totalSkipped, totalFailed)
}

// FailedPermissionMigration Insert a migration entry to the DB for migrating user permissions failed
func (p *postgres) FailedPermissionMigration(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, message, int64(constants.PermissionMigration), int64(constants.Failed), totalSucceeded, totalSkipped, totalFailed)
}

// CompleteMigration Insert a migration entry to the DB for migration completed
func (p *postgres) CompleteMigration(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, "", int64(constants.CompleteMigration), int64(constants.Completed), totalSucceeded, totalSkipped, totalFailed)
}

// CompleteMigration Insert a migration entry to the DB for migration completed
func (p *postgres) FailedMigration(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, migrationId, serverId, message, int64(constants.CompleteMigration), int64(constants.Failed), totalSucceeded, totalSkipped, totalFailed)
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

//GetActiveMigration gets the Migration ID and Migration Status for a server id
func (p *postgres) GetActiveMigration(ctx context.Context, serverId string) (storage.ActiveMigration, error) {
	var m storage.ActiveMigration
	query := "select m.id,t.type from migration m join migration_type t on m.type_id=t.id and t.id < 5000 and m.server_id=$1 order by updated_timestamp desc FETCH FIRST ROW ONLY"
	err := p.db.QueryRowContext(ctx,
		query, serverId).
		Scan(&m.MigrationId, &m.MigrationType)

	if err != nil {
		return storage.ActiveMigration{}, err
	}

	return m, nil
}

//StoreMigrationStage Inserts an entry to the migration_stage
// To use this function, make sure that you should pass the serialized parsed data in []byte
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
