package migrations

import migrations "github.com/chef/automate/api/interservice/infra_proxy/migrations/service"

// InfraProxyMigrationServer stores client
type InfraProxyMigrationServer struct {
	migrationClient migrations.MigrationDataServiceClient
}

// NewInfraProxyHandler initializes InfraProxyServer with client
func NewInfraProxyMigrationHandler(migrationClient migrations.MigrationDataServiceClient) *InfraProxyMigrationServer {
	return &InfraProxyMigrationServer{
		migrationClient: migrationClient,
	}
}
