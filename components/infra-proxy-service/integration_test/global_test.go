package integration_test

import (
	"fmt"
	"os"

	"github.com/chef/automate/components/infra-proxy-service/config"
	"github.com/chef/automate/components/infra-proxy-service/storage/postgres"

	"github.com/pkg/errors"
	"google.golang.org/grpc"
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
func newInfraProxyServer() (*grpc.Server, error) {
	_, haveSvcName := os.LookupEnv(A2_SVC_NAME)
	_, haveSvcPath := os.LookupEnv(A2_SVC_PATH)
	if !(haveSvcName && haveSvcPath) {
		_ = os.Setenv(A2_SVC_NAME, defaultA2ServiceName)
		_ = os.Setenv(A2_SVC_PATH, defaultA2ServicePath)
	}

	fmt.Print("=================")
	uri, err := platform_config.PGURIFromEnvironment(pgDatabaseName)

	if err != nil {
		return nil, errors.Wrap(err, "Failed to get pg uri from environment variables")
	}

	cfg := config.Default()
	cfg.Postgres = config.Postgres{
		URI:        uri,
		Database:   pgDatabaseName,
		SchemaPath: "/src/components/infra-proxy-service/storage/postgres/schema/sql/",
	}

	err = postgres.DestructiveReMigrateForTest(&cfg.Postgres)
	if err != nil {
		return nil, err
	}

	srv := grpc.NewServer(cfg)
	err = srv.ConnectPg()
	if err != nil {
		return nil, err
	}
	return srv, nil
}