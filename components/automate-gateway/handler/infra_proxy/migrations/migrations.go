package migrations

import (
	"context"

	gwreq "github.com/chef/automate/api/external/infra_proxy/migrations/request"
	gwres "github.com/chef/automate/api/external/infra_proxy/migrations/response"
	infra_req "github.com/chef/automate/api/interservice/infra_proxy/migrations/request"
)

// GetMigrationStatus fetches the latest migration status against migration id
func (a *InfraProxyMigrationServer) GetMigrationStatus(ctx context.Context, r *gwreq.GetMigrationStatus) (*gwres.GetMigrationStatus, error) {
	req := &infra_req.GetMigrationStatus{
		MigrationId: r.MigrationId,
	}
	res, err := a.migrationClient.GetMigrationStatus(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.GetMigrationStatus{
		MigrationId:     res.GetMigrationId(),
		MigrationType:   res.GetMigrationType(),
		MigrationStatus: res.GetMigrationStatus(),
	}, nil
}
