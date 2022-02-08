package migrations_test

import (
	"context"
	"errors"
	"testing"

	"github.com/chef/automate/api/external/infra_proxy/migrations/request"

	"github.com/chef/automate/api/external/infra_proxy/migrations/response"

	"github.com/golang/mock/gomock"

	infra_migrations "github.com/chef/automate/api/external/infra_proxy/migrations"
	"github.com/chef/automate/components/infra-proxy-service/test"
)

func TestMigrations(t *testing.T) {
	ctx := context.Background()
	_, serviceRef, _, close, _, _ := test.SetupInfraProxyService(ctx, t)
	infraMigrationMockClient := infra_migrations.NewMockInfraProxyMigrationClient(gomock.NewController(t))

	var migrationID = "Fake id"

	defer close()

	t.Run("GetMigrationStatus", func(t *testing.T) {
		test.ResetState(ctx, t, serviceRef)

		res := &response.GetMigrationStatusResponse{
			MigrationId:     migrationID,
			MigrationType:   "Migration Completed",
			MigrationStatus: "Failed",
		}
		req := request.GetMigrationStatusRequest{
			MigrationId: migrationID,
		}
		t.Run("when a migration id which exist in migration table is submitted, then return the latest status.", func(t *testing.T) {
			infraMigrationMockClient.EXPECT().GetMigrationStatus(gomock.Any(), req, gomock.Any()).Return(res, nil)

		})

		t.Run("when a migration id is not exist in migration table is submitted, then return an error", func(t *testing.T) {
			infraMigrationMockClient.EXPECT().GetMigrationStatus(gomock.Any(), gomock.Any(), gomock.Any()).Return(nil, errors.New("sql: no rows in result set"))

		})
	})

	t.Run("GetStagedData", func(t *testing.T) {
		test.ResetState(ctx, t, serviceRef)

		res := &response.GetStagedDataResponse{
			MigrationId: migrationID,
			StagedData: &response.StagedData{
				OrgsToMigrate: 2,
				OrgsToSkip:    1,
				OrgsToUpdate:  1,
				OrgsToDelete:  1,
				Users: []*response.User{
					{
						Username:         "test1",
						Email:            "abc1@gmail.com",
						DisplayName:      "test1",
						FirstName:        "first1",
						LastName:         "last1",
						MiddleName:       "middle1",
						AutomateUsername: "test1",
						Connector:        "local",
						IsConflicting:    true,
						IsAdmin:          true,
					},
				},
			},
		}
		req := request.GetStagedDataRequest{
			MigrationId: migrationID,
		}
		t.Run("when a migration id which exist in staged table is submitted, then return the staged data", func(t *testing.T) {
			infraMigrationMockClient.EXPECT().GetStagedData(gomock.Any(), req, gomock.Any()).Return(res, nil)
		})

		t.Run("when a migration id is not exist in staged table is submitted, then return an error", func(t *testing.T) {
			infraMigrationMockClient.EXPECT().GetStagedData(gomock.Any(), gomock.Any(), gomock.Any()).Return(nil, errors.New("sql: no rows in result set"))
		})
	})
}
