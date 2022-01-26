package infra_proxy

import (
	migrations "github.com/chef/automate/api/interservice/infra_proxy/migrations/service"
	infra_proxy "github.com/chef/automate/api/interservice/infra_proxy/service"
)

// InfraProxyServer stores client
type InfraProxyServer struct {
	client          infra_proxy.InfraProxyServiceClient
	migrationClient migrations.MigrationDataServiceClient
}

// NewInfraProxyHandler initializes InfraProxyServer with client
func NewInfraProxyHandler(client infra_proxy.InfraProxyServiceClient, migrationClient migrations.MigrationDataServiceClient) *InfraProxyServer {
	return &InfraProxyServer{
		client:          client,
		migrationClient: migrationClient,
	}
}
