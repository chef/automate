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

	defer close()

	t.Run("GetMigrationStatus", func(t *testing.T) {
		test.ResetState(ctx, t, serviceRef)

		res := &response.GetMigrationStatus{
			MigrationId:     "Fake id",
			MigrationType:   "Migration Completed",
			MigrationStatus: "Failed",
		}
		req := request.GetMigrationStatus{
			MigrationId: "Fake id",
		}
		t.Run("when a migration id which exist in migration table is submitted, then return the latest status.", func(t *testing.T) {
			infraMigrationMockClient.EXPECT().GetMigrationStatus(gomock.Any(), req, gomock.Any()).Return(res, nil)

		})

		t.Run("when a migration id is not exist in migration table is submitted, then return an error", func(t *testing.T) {
			infraMigrationMockClient.EXPECT().GetMigrationStatus(gomock.Any(), gomock.Any(), gomock.Any()).Return(nil, errors.New("sql: no rows in result set"))

		})
	})
}
