package integration_test

import (
	"context"

	"github.com/chef/automate/components/infra-proxy-service/config"
	"github.com/chef/automate/components/infra-proxy-service/server"
)

// Global variables
var (
	// A global Infra Proxy Server instance to call any rpc function
	//
	// From any test you can directly call:
	// ```
	// res, err := infraProxy.GetOrgs(ctx, &req)
	// ```
	infraProxy *server.Server
	ctx        = context.Background()
)

type Suite struct{}

// Just returns a new struct. You have to call GlobalSetup() to setup
func NewSuite() *Suite {
	return new(Suite)
}

// GlobalSetup makes all connections.
func (s *Suite) GlobalSetup() error {
	var err error
	// set global infraProxy
	infraProxy, err = newInfraProxyServer()
	if err != nil {
		return err
	}

	// Add role records
	addRoles(totalRecords)

	// Add data bag records
	addDatabagsWithItems(totalRecords)

	// Add environment records
	addEnvironments(totalRecords)

	// Add client records
	addClients(totalRecords)

	return nil
}

// GlobalTeardown is the place where you tear everything down after we have finished
func (s *Suite) GlobalTeardown() {}

// newInfraProxyServer initializes a InfraProxyServer with the default config
func newInfraProxyServer() (*server.Server, error) {
	service, err := config.ConfigFromViper(cFile)
	if err != nil {
		return nil, err
	}

	gRPC := server.NewServer(service)

	return gRPC, nil
}
