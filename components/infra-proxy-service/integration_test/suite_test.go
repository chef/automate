package integration_test

import (
	"context"
	"errors"
	"io/ioutil"
	"os"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/components/infra-proxy-service/config"
	"github.com/chef/automate/components/infra-proxy-service/constants"
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

	return nil
}

// GlobalTeardown is the place where you tear everything down after we have finished
func (s *Suite) GlobalTeardown() {}

func (s *Suite) SetUpAutoDeployChefServer() error {
	ctx := context.Background()
	var serverId string
	server, _ := infraProxy.GetServer(ctx, &request.GetServer{
		Id: "auto-deployed-test-server",
	})

	serverId = server.Server.Id

	// Ignore insertion if already exists
	if server == nil {

		server, err := infraProxy.CreateServer(ctx, &request.CreateServer{
			Id:        "auto-deployed-test-server",
			Name:      "auto-deployed-test-server",
			IpAddress: "127.0.0.1",
			Fqdn:      defaultServerHost(),
		})

		if err != nil {
			return err
		}

		serverId = server.Server.Id
	}

	pemFile, err := defaultServerAdminKey()
	if err != nil {
		return errors.New("newerror")
	}

	_, err = infraProxy.CreateOrg(ctx, &request.CreateOrg{
		Id:        "auto-deployed-test-org",
		ServerId:  serverId,
		Name:      "test",
		AdminUser: "pivotal",
		AdminKey:  pemFile,
		Projects:  []string{constants.UnassignedProjectID},
	})

	return err
}

// newInfraProxyServer initializes a InfraProxyServer with the default config
func newInfraProxyServer() (*server.Server, error) {
	service, err := config.ConfigFromViper(cFile)
	if err != nil {
		return nil, err
	}

	gRPC := server.NewServer(service)

	return gRPC, nil
}

func defaultServerHost() string {
	cName := os.Getenv("CONTAINER_HOSTNAME")
	serverHost := "a2-dev.test"
	if cName != "" {
		serverHost = cName + serverHost
	}

	return serverHost
}

func defaultServerAdminKey() (string, error) {
	file, err := os.Open("/hab/svc/automate-cs-oc-erchef/data/pivotal.pem")
	if err != nil {
		return "", err
	}

	content, err := ioutil.ReadAll(file)
	if err != nil {
		return "", err
	}

	return string(content), nil
}
