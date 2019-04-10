//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package integration_test

import (
	"os"
	"testing"

	"github.com/gofrs/uuid"

	cElastic "github.com/chef/automate/components/config-mgmt-service/backend/elastic"
	"github.com/chef/automate/components/config-mgmt-service/config"
	grpc "github.com/chef/automate/components/config-mgmt-service/grpcserver"
)

// Global variables
var (
	// The elasticsearch URL is coming from the environment variable ELASTICSEARCH_URL
	elasticsearchUrl = os.Getenv("ELASTICSEARCH_URL")

	// This suite variable will be available for every single test as long as they
	// belong to the 'integration_test' package.
	suite = NewSuite(elasticsearchUrl)

	// A global CfgMgmt Server instance to call any rpc function
	//
	// From any test you can directly call:
	// ```
	// res, err := cfgmgmt.GetNodesCounts(ctx, &req)
	// ```
	cfgmgmt = newCfgMgmtServer()
)

// newCfgMgmtServer initialices a CfgMgmtServer with the default config
// and points to our preferred backend that is elasticsearch.
//
// NOTE: This function expects ES to be already up and running.
// (@afiune) We are going to start ES from the studio
func newCfgMgmtServer() *grpc.CfgMgmtServer {
	cfg := config.Default()
	cfg.SetBackend(cElastic.New(elasticsearchUrl))
	return grpc.NewCfgMgmtServer(cfg)
}

// newUUID generates a new UUID and returns it as a string
func newUUID() string {
	return uuid.Must(uuid.NewV4()).String()
}

// TestMain allow us to run a setup before running our tests and also
// teardown everything after we have finished testing. Great place to
// initialize our ES indices. (Check out 'suite_test.go')
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
