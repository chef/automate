//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2019, Chef Software Inc.
//

package integration_test

import (
	"fmt"
	"os"
	"time"

	"github.com/golang/protobuf/proto"

	"github.com/chef/automate/api/external/habitat"
	"github.com/chef/automate/components/applications-service/pkg/config"
	"github.com/chef/automate/components/applications-service/pkg/ingester"
	ingest "github.com/chef/automate/components/applications-service/pkg/ingester/v1"
	"github.com/chef/automate/components/applications-service/pkg/server"
	"github.com/chef/automate/components/applications-service/pkg/storage"
	"github.com/chef/automate/components/applications-service/pkg/storage/postgres"
	platform_config "github.com/chef/automate/lib/platform/config"
)

// Suite helps you manipulate various stages of your tests, it provides common
// functionality like; Global setup and teardown functions. If you have some
// functionality that is repetitive across multiple tests, consider putting it
// here so that we have them available globally
//
// This struct holds:
// * ApplicationsServer: This is our main RPC server we want to test against
// * StorageClient: Lets us manipulate our database to add or remove things from it
// * Ingester: The mechanism to ingest messages to our system
type Suite struct {
	ApplicationsServer *server.ApplicationsServer
	Ingester           ingester.Client
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
	uri, err := platform_config.PGURIFromEnvironment(database)
	if err != nil {
		fmt.Printf("Failed to get pg uri: %s\n", err)
		os.Exit(1)
	}

	var (
		s = new(Suite)
		c = &config.Applications{
			Postgres: config.Postgres{
				URI:        uri,
				Database:   database,
				SchemaPath: "/src/components/applications-service/pkg/storage/postgres/schema/sql",
			},
		}
	)
	dbClient, err := postgres.New(&c.Postgres)
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

	// A global Ingester instance to ingest any "message" into our system
	//
	// From any test you can directly call:
	// ```
	// suite.Ingester.IngestMessage(msg)
	// suite.WaitForEventsToProcess(1)
	// ```
	s.Ingester = ingest.New(c, s.StorageClient)

	// Start processing messages as they are sent to the ingest events channel
	go s.Ingester.Run()

	// A global ApplicationsServer instance to call any rpc function
	//
	// From any test you can directly call:
	// ```
	// res, err := suite.ApplicationsServer.GetServicesHealthCounts(ctx, &req)
	// ```
	s.ApplicationsServer = server.New(s.StorageClient, s.Ingester)

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

// IngestService ingests a single HealthCheckEvent message into the database
func (s *Suite) IngestService(event *habitat.HealthCheckEvent) {
	eventsProcessed := s.Ingester.EventsProcessed()
	bytes, err := proto.Marshal(event)
	if err != nil {
		fmt.Printf("Error trying to ingest hab service event: %s\n", err)
	}
	s.Ingester.IngestMessage(bytes)
	s.WaitForEventsToProcess(eventsProcessed + 1)
}

func (s *Suite) IngestServiceViaStorageClient(event *habitat.HealthCheckEvent) {
	err := s.StorageClient.IngestHealthCheckEvent(event)
	if err != nil {
		fmt.Printf("Error trying to ingest hab service event: %s\n", err)
	}
}

// GetServiceGroups retrieve the service_groups from the database
func (s *Suite) GetServiceGroups() []*storage.ServiceGroupDisplay {
	sgList, err := s.StorageClient.GetServiceGroups("name", true, 1, 100, nil)
	if err != nil {
		fmt.Printf("Error trying to retrieve service_groups from db: %s\n", err)
	}
	return sgList
}

// GetServices retrieve the services from the database
func (s *Suite) GetServices() []*storage.Service {
	svcList, err := s.StorageClient.GetServices("name", true, 1, 100, nil)
	if err != nil {
		fmt.Printf("Error trying to retrieve services from db: %s\n", err)
	}
	return svcList
}

func (s *Suite) GetServicesCountForStatsEndpoint() int32 {
	count, err := s.StorageClient.GetServicesCount()
	if err != nil {
		fmt.Printf("Error trying to retrieve services count from db: %s\n", err)
	}
	return count
}

func (s *Suite) GetServiceGroupsCountForStatsEndpoint() int32 {
	count, err := s.StorageClient.GetServiceGroupsCount()
	if err != nil {
		fmt.Printf("Error trying to retrieve service groups count from db: %s\n", err)
	}
	return count
}

// IngestServices ingests multiple HealthCheckEvent messages into the database
func (s *Suite) IngestServices(events []*habitat.HealthCheckEvent) {
	for _, e := range events {
		s.IngestService(e)
	}
}

// Ingest messages through the Ingester client, waits for all events to be processed
func (s *Suite) IngestMessagesViaIngester(events ...*habitat.HealthCheckEvent) {

	var (
		// Store the number of events that the ingester has already processed
		eventsProcessed = suite.Ingester.EventsProcessed()

		// Number of events to process
		eventsToProcess = int64(len(events))
	)

	for _, e := range events {
		bytes, err := proto.Marshal(e)
		if err != nil {
			fmt.Printf("Error trying to marshal event: %s\n", err)
			continue
		}
		suite.Ingester.IngestMessage(bytes)
	}

	// Wait until all events have been processed
	s.WaitForEventsToProcess(eventsProcessed + eventsToProcess)
}

// Lock function to wait for a number of events to process through the ingester client
func (s *Suite) WaitForEventsToProcess(n int64) {
	wait := 0

	for {
		if suite.Ingester.EventsProcessed() >= n {
			break
		}

		time.Sleep(10 * time.Millisecond)

		wait = wait + 10

		if wait >= maxWaitTimeMs {
			fmt.Printf("Error: wait time exceeded (time:%dms) [WaitForEventsToProcess]\n", wait)
			os.Exit(1)
		}
	}
}
