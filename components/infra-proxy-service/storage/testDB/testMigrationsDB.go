package testDB

import (
	"context"

	"github.com/chef/automate/components/infra-proxy-service/storage"
	"github.com/pkg/errors"
)

type MigrationDB struct {
	MigrationStatus []storage.Migration
	NeedError       bool
}

func (m MigrationDB) StartMigration(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) StartFileUpload(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) CompleteFileUpload(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) FailedFileUpload(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) StartUnzip(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) CompleteUnzip(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) FailedUnzip(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) StartOrgParsing(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	if m.NeedError {
		return storage.Migration{}, errors.New("Failed to update status")
	}
	return storage.Migration{
		ID:       "mig1",
		ServerID: "serverId",
		TypeID:   100,
	}, nil
}

func (m MigrationDB) CompleteOrgParsing(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	if m.NeedError {
		return storage.Migration{}, errors.New("Failed to update status")
	}
	return storage.Migration{
		ID:       "mig1",
		ServerID: "serverId",
		TypeID:   100,
	}, nil
}

func (m MigrationDB) FailedOrgParsing(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	if m.NeedError {
		return storage.Migration{}, errors.New("Failed to update status")
	}
	return storage.Migration{
		ID:       "mig1",
		ServerID: "serverId",
		TypeID:   100,
	}, nil
}

func (m MigrationDB) StartUsersParsing(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) CompleteUsersParsing(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) FailedUsersParsing(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) StartUserAssociationParsing(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) CompleteUserAssociationParsing(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) FailedUserAssociationParsing(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) StartPermissionParsing(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) CompletePermissionParsing(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) FailedPermissionParsing(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) StartCreatePreview(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	if m.NeedError {
		return storage.Migration{}, errors.New("Failed to update status")
	}
	return storage.Migration{
		ID:       "mig1",
		ServerID: "serverId",
		TypeID:   100,
	}, nil
}

func (m MigrationDB) CompleteCreatePreview(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	if m.NeedError {
		return storage.Migration{}, errors.New("Failed to update status")
	}
	return storage.Migration{
		ID:       "mig1",
		ServerID: "serverId",
		TypeID:   100,
	}, nil
}

func (m MigrationDB) FailedCreatePreview(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	if m.NeedError {
		return storage.Migration{}, errors.New("Failed to update status")
	}
	return storage.Migration{
		ID:       "mig1",
		ServerID: "serverId",
		TypeID:   100,
	}, nil
}

func (m MigrationDB) StartOrgMigration(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) CompleteOrgMigration(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) FailedOrgMigration(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) StartUserMigration(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) CompleteUserMigration(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) FailedUserMigration(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) StartAssociation(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) CompleteAssociation(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) FailedAssociation(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) StartPermissionMigration(ctx context.Context, migrationId, serverId string) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) CompletePermissionMigration(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) FailedPermissionMigration(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) CompleteMigration(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) FailedMigration(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	if m.NeedError {
		return storage.Migration{}, errors.New("Failed to update status")
	}
	return storage.Migration{
		ID:       "mig1",
		ServerID: "serverId",
		TypeID:   100,
	}, nil
}

func (m MigrationDB) CancelMigration(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) FailedCancelMigration(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (storage.Migration, error) {
	panic("implement me")
}

func (m MigrationDB) StoreMigrationStage(ctx context.Context, migrationId string, parsedData interface{}) (storage.MigrationStage, error) {
	if m.NeedError {
		return storage.MigrationStage{}, errors.New("Failed to insert staged data")
	}
	return storage.MigrationStage{
		ID:          "1",
		MigrationID: "mig1",
	}, nil

}

func (m MigrationDB) GetMigrationStage(ctx context.Context, migrationId string) (storage.MigrationStage, error) {
	panic("implement me")
}

func (m MigrationDB) DeleteMigrationStage(ctx context.Context, migrationId string) (storage.MigrationStage, error) {
	panic("implement me")
}

func (m MigrationDB) GetActiveMigration(ctx context.Context, serverId string) (storage.MigrationStatus, error) {
	panic("implement me")
}

func (m MigrationDB) GetMigrationStatus(ctx context.Context, migrationId string) (storage.MigrationStatus, error) {
	panic("implement me")
}
