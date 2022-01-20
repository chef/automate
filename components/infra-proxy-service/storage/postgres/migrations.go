package postgres

import (
	"context"
	"encoding/json"

	"github.com/chef/automate/components/infra-proxy-service/constants"
	"github.com/chef/automate/components/infra-proxy-service/storage"
	"github.com/gofrs/uuid"
)

// StartZipParsing Insert a migration entry to the DB for zip parsing started
func (p *postgres) StartZipParsing(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	return p.insertMigration(ctx, uuid.Must(uuid.NewV4()).String(), migrationId, serverId, int64(constants.StartZipParsing), int64(constants.InProgress), 0, 0, 0)
}

// CompleteZipParsing Insert a migration entry to the DB for zip parsing completed
func (p *postgres) CompleteZipParsing(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, uuid.Must(uuid.NewV4()).String(), migrationId, serverId, int64(constants.CompleteZipParsing), int64(constants.Completed), totalSucceeded, totalSkipped, totalFailed)
}

// StartOrgMigration Insert a migration entry to the DB for org migration started
func (p *postgres) StartOrgMigration(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	return p.insertMigration(ctx, uuid.Must(uuid.NewV4()).String(), migrationId, serverId, int64(constants.StartOrgMigration), int64(constants.InProgress), 0, 0, 0)
}

// CompleteOrgMigration Insert a migration entry to the DB for org migration completed
func (p *postgres) CompleteOrgMigration(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, uuid.Must(uuid.NewV4()).String(), migrationId, serverId, int64(constants.CompleteOrgMigration), int64(constants.Completed), totalSucceeded, totalSkipped, totalFailed)
}

// StartUserMigration Insert a migration entry to the DB for user migration started
func (p *postgres) StartUserMigration(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	return p.insertMigration(ctx, uuid.Must(uuid.NewV4()).String(), migrationId, serverId, int64(constants.StartUserMigration), int64(constants.InProgress), 0, 0, 0)
}

// CompleteUserMigration Insert a migration entry to the DB for user migration completed
func (p *postgres) CompleteUserMigration(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, uuid.Must(uuid.NewV4()).String(), migrationId, serverId, int64(constants.CompleteUserMigration), int64(constants.Completed), totalSucceeded, totalSkipped, totalFailed)
}

// StartAssciation Insert a migration entry to the DB for Association of users to orgs started
func (p *postgres) StartAssciation(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	return p.insertMigration(ctx, uuid.Must(uuid.NewV4()).String(), migrationId, serverId, int64(constants.StartAssciation), int64(constants.InProgress), 0, 0, 0)
}

// CompleteAssciation Insert a migration entry to the DB for Association of users to orgs completed
func (p *postgres) CompleteAssciation(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, uuid.Must(uuid.NewV4()).String(), migrationId, serverId, int64(constants.CompleteAssciation), int64(constants.Completed), totalSucceeded, totalSkipped, totalFailed)
}

// StartPermissionMigration Insert a migration entry to the DB for migrating user permissions started
func (p *postgres) StartPermissionMigration(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	return p.insertMigration(ctx, uuid.Must(uuid.NewV4()).String(), migrationId, serverId, int64(constants.StartPermissionMigration), int64(constants.InProgress), 0, 0, 0)
}

// CompletePermissionMigration Insert a migration entry to the DB for migrating user permissions completed
func (p *postgres) CompletePermissionMigration(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	return p.insertMigration(ctx, uuid.Must(uuid.NewV4()).String(), migrationId, serverId, int64(constants.CompletePermissionMigration), int64(constants.Completed), totalSucceeded, totalSkipped, totalFailed)
}

//insertMigration Inserts an entry to the DB
func (p *postgres) insertMigration(ctx context.Context, id, migrationId, serverId string, migTypeId, migStatusId, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {

	var m storage.Migration
	var mByte []byte

	query := "SELECT insert_migration($1,$2, $3, $4, $5, $6, $7, $8)"
	row := p.db.QueryRowContext(ctx, query, id, migrationId, serverId, migTypeId, migStatusId, totalSucceeded, totalSkipped, totalFailed)
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
