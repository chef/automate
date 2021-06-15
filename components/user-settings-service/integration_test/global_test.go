package integration_test
import (
	"fmt"
	//"github.com/chef/automate/lib/tls/certs"
	"os"
	"testing"

	"github.com/chef/automate/components/user-settings-service/pkg/config"
	"github.com/chef/automate/components/user-settings-service/pkg/server"
	"github.com/chef/automate/components/user-settings-service/pkg/storage/postgres"
)

// Global variables
var (
	// The postgresql URL is coming from the environment variable POSTGRESQL_URL
	postgresqlUrl = os.Getenv("POSTGRESQL_URL")

	db = createDatabaseObject()

	// This suite variable will be available for every single test as long as they
	// belong to the 'integration_test' package.
	suite = NewSuite(db)

	// A global CfgMgmt Server instance to call any rpc function
	//
	// From any test you can directly call:
	// ```
	// res, err := cfgmgmt.GetNodesCounts(ctx, &req)
	// ```
	userSettingsServer = server.New(db, db)
)

func createDatabaseObject() *postgres.DB {
	connectionString := "postgresql://user_settings@" + postgresqlUrl +
		"/user-settings_service?sslmode=verify-ca&sslcert=/hab/svc/user-settings-service/config/service.crt&sslkey=/hab/svc/user-settings-service/config/service.key&sslrootcert=/hab/svc/user-settings-service/config/root_ca.crt"
	postgresConfig := config.Postgres{URI: connectionString, SchemaPath: "/src/components/user-settings-service/pkg/storage/postres/schema/sql"}
	//db, err := postgres.Connect(&postgresConfig, "75e79c17ae62445e9771cd13fc4216f4")
	db, err := postgres.Connect(&postgresConfig)
	if err != nil {
		fmt.Printf("Could not create postgresql client from '%s': %s\n", connectionString, err)
		os.Exit(1)
	}

	return db
}

// TestMain allow us to run a setup before running our tests and also
// teardown everything after we have finished testing.
//
// => Docs: https://golang.org/pkg/testing/#hdr-Main
func TestMain(m *testing.M) {
	// Global Setup hook: Here is where you can initialize anythings you need
	// for your tests to run, things like; Initialize ES indices, insert
	// nodes or runs, etc.
	suite.GlobalSetup()

	// Execute the test suite and record the exit code
	exitCode := m.Run()

	// Teardown hook: It says it all, this hook should clean documents
	// from ES so that the next test can run on a clean env.
	suite.GlobalTeardown()

	// call with result of m.Run()
	os.Exit(exitCode)
}
