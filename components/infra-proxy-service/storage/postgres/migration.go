package postgres

import (
	"context"

	"github.com/chef/automate/components/infra-proxy-service/storage"
	"github.com/gofrs/uuid"
)

// StoreMigration saves a migration entry to the DB.
func (p *postgres) StoreMigration(ctx context.Context, serverId, migType, migStatus string) (storage.Migration, error) {

	// get the type and status ID from migration_type and migration_status tables
	migrationType, err := p.getMigrationType(ctx, migType)
	if err != nil {
		return storage.Migration{}, p.processError(err)
	}
	migrationStatus, err := p.getMigrationStatus(ctx, migStatus)
	if err != nil {
		return storage.Migration{}, p.processError(err)
	}
	id, err := uuid.NewV4()
	if err != nil {
		return storage.Migration{}, p.processError(err)
	}
	return p.insertMigration(ctx, id.String(), serverId, migrationType.ID, migrationStatus.ID)
}

func (p *postgres) insertMigration(ctx context.Context, id, serverId, migTypeId, migStatusId string) (storage.Migration, error) {

	var m storage.Migration
	err := p.db.QueryRowContext(ctx,
		`INSERT INTO migration (id, type_id, status_id, server_id, created_at, updated_at)
		VALUES ($1, $2, $3, $4, now(), now())
		RETURNING id, type_id, status_id, server_id, created_at, updated_at`,
		id, migTypeId, migStatusId, serverId).
		Scan(&m.ID, &m.TypeID, &m.StatusID, &m.ServerID, &m.CreatedAt, &m.UpdatedAt)
	if err != nil {
		return storage.Migration{}, p.processError(err)
	}

	return m, nil
}

// GetServer fetches a migration by ID.
func (p *postgres) GetMigration(ctx context.Context, id string) (storage.Migration, error) {
	return p.getMigration(ctx, p.db, id)
}

func (p *postgres) getMigration(ctx context.Context, q querier, id string) (storage.Migration, error) {
	var m storage.Migration
	err := q.QueryRowContext(ctx,
		`SELECT m.id,m.type_id, m.status_id, 
		m.total_succeeded, m.total_skipped,m.total_failed,
		m.created_at,m.updated_at
		FROM migration m
		WHERE m.id = $1`, id).
		Scan(&m.ID, &m.TypeID, &m.StatusID, &m.TotalSucceeded, &m.TotalSkipped, &m.TotalFailed, &m.CreatedAt, &m.UpdatedAt)
	if err != nil {
		return storage.Migration{}, p.processError(err)
	}
	return m, nil
}

// EditMigration does a full update on a database server.
func (p *postgres) EditMigration(ctx context.Context, id, typeId, migStatus string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	var m storage.Migration

	migrationStatus, err := p.getMigrationStatus(ctx, migStatus)
	if err != nil {
		return storage.Migration{}, p.processError(err)
	}

	err = p.db.QueryRowContext(ctx,
		`UPDATE migration
		SET type_id = $2, status_id = $3, total_succeeded = $4, total_skipped = $5, total_failed = $6, updated_at = now()
		WHERE id = $1
		RETURNING id, type_id, status_id, total_succeeded, total_skipped, total_failed, created_at, updated_at`,
		id, typeId, migrationStatus.ID, totalSucceeded, totalSkipped, totalFailed).
		Scan(&m.ID, &m.TypeID, &migrationStatus.ID, &m.TotalSucceeded, &m.TotalSkipped, &m.TotalFailed, &m.CreatedAt, &m.UpdatedAt)
	if err != nil {
		return storage.Migration{}, p.processError(err)
	}

	return m, nil
}

//getMigrationType fetched migration type id from migration_type by type
func (p *postgres) getMigrationType(ctx context.Context, migType string) (storage.MigrationType, error) {
	var mType storage.MigrationType

	err := p.db.QueryRowContext(ctx,
		`SELECT m.id, m.type FROM migration_type m
		WHERE m.type = $1`, migType).
		Scan(&mType.ID, &mType.MigType)
	if err != nil {
		return storage.MigrationType{}, p.processError(err)
	}
	return mType, nil
}

//getMigrationStatus fetched migration status id from migration_status by status
func (p *postgres) getMigrationStatus(ctx context.Context, migStatus string) (storage.MigrationStatus, error) {
	var mStatus storage.MigrationStatus

	err := p.db.QueryRowContext(ctx,
		`SELECT m.id, m.status_message FROM migration_status m
		WHERE m.status_message = $1`, migStatus).
		Scan(&mStatus.ID, &mStatus.MigStatus)
	if err != nil {
		return storage.MigrationStatus{}, p.processError(err)
	}
	return mStatus, nil
}
