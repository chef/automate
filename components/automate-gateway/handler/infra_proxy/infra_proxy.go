package infra_proxy

import (
	infra_proxy "github.com/chef/automate/api/interservice/infra_proxy/service"
)

// InfraProxyServer stores client
type InfraProxyServer struct {
	client infra_proxy.InfraProxyServiceClient
}

// NewInfraProxyHandler initializes InfraProxyServer with client
func NewInfraProxyHandler(client infra_proxy.InfraProxyServiceClient) *InfraProxyServer {
	return &InfraProxyServer{
		client: client,
	}
}
