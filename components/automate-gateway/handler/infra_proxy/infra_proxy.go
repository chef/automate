package infra_proxy

import (
	"context"

	version "github.com/chef/automate/api/external/common/version"
	infra_proxy "github.com/chef/automate/api/interservice/infra_proxy/service"
)

// InfraProxyServer stores client
type InfraProxyServer struct {
	client infra_proxy.InfraProxyClient
}

// NewInfraProxyHandler initializes InfraProxyServer with client
func NewInfraProxyHandler(client infra_proxy.InfraProxyClient) *InfraProxyServer {
	return &InfraProxyServer{
		client: client,
	}
}

// GetVersion fetches the version of infra proxy service
func (a *InfraProxyServer) GetVersion(ctx context.Context, e *version.VersionInfoRequest) (*version.VersionInfo, error) {
	return a.client.GetVersion(ctx, e)
}
