//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2019, Chef Software Inc.
//

package integration_test

import (
	"fmt"
	"os"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/components/applications-service/pkg/config"
	"github.com/chef/automate/components/applications-service/pkg/server"
	"github.com/chef/automate/components/applications-service/pkg/storage"
	"github.com/chef/automate/components/applications-service/pkg/storage/postgres"
	"github.com/chef/automate/lib/platform"
)

// Suite helps you manipulate various stages of your tests, it provides common
// functionality like; Global setup and teardown functions. If you have some
// functionality that is repetitive across multiple tests, consider putting it
// here so that we have them available globally
//
// This struct holds:
// * ApplicationsServer: This is our main RPC server we want to test against
// * StorageClient: Lets us manipulate our database to add or remove things from it
type Suite struct {
	ApplicationsServer *server.ApplicationsServer
	StorageClient      storage.Client
}

// Initialize the test suite
//
// This verifies the connectivity with Postgres; if we couldn't
// connect, we do not start the tests and print an error message
//
// NOTE: (@afiune) This function expects postgres to be already
// up and running, it should be started from within the studio.
func NewSuite(database string) *Suite {
	uri, err := platform.PGURIFromEnvironment(database)
	if err != nil {
		fmt.Printf("Failed to get pg uri: %s\n", err)
		os.Exit(1)
	}

	var (
		s = new(Suite)
		c = config.Postgres{
			URI:        uri,
			Database:   database,
			SchemaPath: "/src/components/applications-service/pkg/storage/postgres/schema/sql",
		}
	)
	dbClient, err := postgres.New(&c)
	if err != nil {
		fmt.Printf("Could not create postgres client: %s\n", err)
		os.Exit(1)
	}

	// A global Storage Client to call any storage function
	//
	// From any test you can directly call:
	// ```
	// svcsHealthCounts, err := suite.StorageClient.GetServicesHealthCounts()
	// ```
	s.StorageClient = dbClient

	// A global ApplicationsServer instance to call any rpc function
	//
	// From any test you can directly call:
	// ```
	// res, err := suite.ApplicationsServer.GetServicesHealthCounts(ctx, &req)
	// ```
	s.ApplicationsServer = server.New(s.StorageClient)

	return s
}

// GlobalSetup prepare anything that we need before executing all our test suite
func (s *Suite) GlobalSetup() {
	// Make sure our database is empty for our integration tests
	s.DeleteDataFromStorage()
}

// GlobalTeardown tear everything down after finishing executing all our test suite
func (s *Suite) GlobalTeardown() {
}

// DeleteDataFromStorage will drop the entire database, you can use this function
// to initialize tests, empty the db after a test ran (with a defer clause), etc.
func (s *Suite) DeleteDataFromStorage() {
	err := s.StorageClient.EmptyStorage()
	if err != nil {
		fmt.Printf("Error trying to delete data from storage: %s\n", err)
	}
}

// IngestService ingests a single HabService message into the database
func (s *Suite) IngestService(event *applications.HabService) {
	err := s.StorageClient.IngestHabEvent(event)
	if err != nil {
		fmt.Printf("Error trying to ingest hab service event: %s\n", err)
	}
}

// IngestServices ingests multiple HabService messages into the database
func (s *Suite) IngestServices(events []*applications.HabService) {
	for _, e := range events {
		s.IngestService(e)
	}
}
