//
//  Author:: Salim Afiune <afiune@chef.io>, Gina Peers <gpeers@chef.io>
//  Copyright:: Copyright 2018, Chef Software Inc.
//

package integration_test

import (
	"os"
	"testing"

	"net"

	"context"

	"github.com/gofrs/uuid"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"

	dls "github.com/chef/automate/api/interservice/data_lifecycle"
	automate_feed "github.com/chef/automate/components/compliance-service/api/automate-feed"
	"github.com/chef/automate/components/compliance-service/examples/helpers"
)

// Global variables
var (
	// The elasticsearch URL comes from the environment variable ELASTICSEARCH_URL
	elasticsearchUrl = os.Getenv("ELASTICSEARCH_URL")

	// Suite variable is available to all tests that
	// belong to the 'integration_test' package

	testSuite = NewSuite(elasticsearchUrl)

	// Global Feed Server instance.
	// From any test, make gRPC calls, e.g.:
	// ```
	// res, err := feedService.GetFeed(ctx, &req)
	// ```
	feedService = testSuite.feedServer

	purgeClient dls.DataLifecycleClient
)

// NewUUID generates a new UUID and returns it as a string
func newUUID() string {
	return uuid.Must(uuid.NewV4()).String()
}

func GetTestSuite() *Suite {
	return testSuite
}

// TestMain allows us to set up a test fixture before the test run
// and tear down afterward.
//
// => Docs: https://golang.org/pkg/testing/#hdr-Main
func TestMain(m *testing.M) {
	logrus.Infof("ELASTIC URL is: %s", elasticsearchUrl)
	// Global Setup hook: initialize anything required for the test
	// run, e.g., initializing ES indices, inserting test data, etc.
	testSuite.GlobalSetup()

	// start gRPC server
	lis, err := net.Listen("tcp", "localhost:50051")
	if err != nil {
		logrus.Fatalf("failed to listen: %v", err)
	}
	grpcServer := grpc.NewServer()
	automate_feed.RegisterFeedServiceServer(grpcServer, testSuite.feedServer)
	logrus.Infof("feed server listening on port 50051")
	go grpcServer.Serve(lis)
	defer grpcServer.Stop()

	pwd, _ := os.Getwd()
	logrus.Infof("pwd is %s", pwd)
	// get the datalifecycle client so we can test the feed purge
	connFactory := helpers.SecureConnFactoryHab()
	conn, err := connFactory.DialContext(context.Background(), "data-lifecycle-service", "localhost:10129")
	if err != nil {
		logrus.Error(err)
		os.Exit(1)
	}

	purgeClient = dls.NewDataLifecycleClient(conn)
	defer conn.Close()

	// Execute the test suite and record the exit code
	exitCode := m.Run()

	// Teardown hook: clean up any test fixtures created
	// for the test so that tests run independently
	testSuite.GlobalTeardown()

	// call with result of m.Run()
	os.Exit(exitCode)
}
