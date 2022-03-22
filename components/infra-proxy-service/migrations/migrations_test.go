package migrations

import (
	"context"
	"errors"
	"reflect"
	"testing"

	"github.com/chef/automate/api/interservice/infra_proxy/migrations/request"

	"github.com/chef/automate/api/interservice/infra_proxy/migrations/response"

	"github.com/chef/automate/components/infra-proxy-service/service"
	"github.com/chef/automate/components/infra-proxy-service/storage/testDB"
)

func TestGetMigrationStatus(t *testing.T) {
	type args struct {
		ctx       context.Context
		server    MigrationServer
		req       *request.GetMigrationStatusRequest
		NeedError bool
	}

	tests := []struct {
		name      string
		args      args
		wantError error
		want1     *response.GetMigrationStatusResponse
	}{
		{name: "Test Get migration status, should return the latest status of migration id", args: args{ctx: context.Background(), server: MigrationServer{service: &service.Service{Migration: &testDB.MigrationDB{}}}, req: &request.GetMigrationStatusRequest{MigrationId: "mig1"}}, wantError: nil, want1: &response.GetMigrationStatusResponse{MigrationId: "mig1", MigrationType: "Creating Preview", MigrationStatus: "Completed"}},
		{name: "Test Error from Get migration status, if status is not present for migration id", args: args{ctx: context.Background(), server: MigrationServer{service: &service.Service{Migration: &testDB.MigrationDB{}}}, req: &request.GetMigrationStatusRequest{MigrationId: "mig1"}}, wantError: errors.New("Failed to fetch migration status"), want1: &response.GetMigrationStatusResponse{MigrationId: "mig1", MigrationType: "Creating Preview", MigrationStatus: "Completed"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			got, err := tt.args.server.GetMigrationStatus(tt.args.ctx, tt.args.req)
			if err != nil && err.Error() != tt.wantError.Error() {
				t.Errorf("GetMigrationStatus() err = %v, want %v", err, tt.wantError)
			}
			if !reflect.DeepEqual(got, tt.want1) {
				t.Errorf("GetMigrationStatus() got = %v, want %v", got, tt.want1)
			}

		})

	}
}
