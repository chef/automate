package integration_test

import (
	"os"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/infra-proxy-service/config"
	"github.com/chef/automate/components/infra-proxy-service/server"
	"github.com/chef/automate/components/infra-proxy-service/service"
	platform_config "github.com/chef/automate/lib/platform/config"
)

// Global variables
var (
	// This suite variable will be available for every single test as long as they
	// belong to the 'integration_test' package.
	suite = NewSuite()
)

// GlobalSetup makes backend connections to postgres. It also sets
// global vars to usable values.
func (s *Suite) GlobalSetup() error {
	// set global infraProxy
	var err error
	infraProxy, err = newInfraProxyServer()

	if err != nil {
		return err
	}

	return nil
}


// newInfraProxyServer initializes a InfraProxyServer with the default config
func newInfraProxyServer() (*server.Server, error) {
	_, haveSvcName := os.LookupEnv(A2_SVC_NAME)
	_, haveSvcPath := os.LookupEnv(A2_SVC_PATH)
	if !(haveSvcName && haveSvcPath) {
		_ = os.Setenv(A2_SVC_NAME, defaultA2ServiceName)
		_ = os.Setenv(A2_SVC_PATH, defaultA2ServicePath)
	}

	uri, err := platform_config.PGURIFromEnvironment(pgDatabaseName)

	if err != nil {
		return nil, errors.Wrap(err, "Failed to get pg uri from environment variables")
	}

	cfg := config.Default()
	cfg.PGURL = uri
	cfg.Database = pgDatabaseName

	service, err := service.Start(cfg.LogLevel, migrationConfig, nil, nil, nil)
	if err != nil {
		fail(errors.Wrap(err, "could not initialize storage"))
	}

	fail(server.GRPC(cfg.GRPC, service))

	srv := server.NewServer(cfg)
	return srv, nil
}