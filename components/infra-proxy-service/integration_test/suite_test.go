package integration_test

import (
	grpc "github.com/chef/automate/components/infra-proxy-service/server"
)

// Global variables
var (
	
	// A global Infra Proxy Server instance to call any rpc function
	//
	// From any test you can directly call:
	// ```
	// res, err := infraProxy.GetOrgs(ctx, &req)
	// ```
	infraProxy *grpc.Server
)

const (
	pgDatabaseName       = "chef_infra_proxy"
	A2_SVC_NAME          = "A2_SVC_NAME"
	A2_SVC_PATH          = "A2_SVC_PATH"
	defaultA2ServiceName = "infra-proxy-service"
	defaultA2ServicePath = "/hab/svc/infra-proxy-service"
)


type Suite struct {}

// Just returns a new struct. You have to call GlobalSetup() to setup
func NewSuite() *Suite {
	return new(Suite)
}