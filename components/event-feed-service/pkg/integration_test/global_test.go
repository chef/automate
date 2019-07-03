package integration_test

import (
	"context"
	"os"
	"path"
	"testing"

	"github.com/chef/automate/api/interservice/data_lifecycle"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/certs"
	"github.com/gofrs/uuid"
	log "github.com/sirupsen/logrus"
)

// Global variables
var (
	// The elasticsearch URL comes from the environment variable ELASTICSEARCH_URL
	elasticsearchUrl = os.Getenv("ELASTICSEARCH_URL")

	// Suite variable is available to all tests that
	// belong to the 'integration_test' package

	testSuite = NewSuite(elasticsearchUrl)

	dataLifecycleClient data_lifecycle.DataLifecycleClient
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
	log.Infof("ELASTIC URL is: %s", elasticsearchUrl)
	// Global Setup hook: initialize anything required for the test
	// run, e.g., initializing ES indices, inserting test data, etc.
	testSuite.GlobalSetup()

	// get the datalifecycle client so we can test the feed purge
	connFactory := secureConnFactoryHab()
	conn, err := connFactory.DialContext(context.Background(), "data-lifecycle-service", "localhost:10129")
	if err != nil {
		log.Error(err)
		os.Exit(1)
	}

	dataLifecycleClient = data_lifecycle.NewDataLifecycleClient(conn)
	defer conn.Close()

	// Execute the test suite and record the exit code
	exitCode := m.Run()

	// Teardown hook: clean up any test fixtures created
	// for the test so that tests run independently
	testSuite.GlobalTeardown()

	// call with result of m.Run()
	os.Exit(exitCode)
}

func secureConnFactoryHab() *secureconn.Factory {
	certs := loadCertsHab()
	return secureconn.NewFactory(*certs)
}

// uses the certs in a running hab env
func loadCertsHab() *certs.ServiceCerts {
	dirname := "/hab/svc/event-feed-service/config"
	log.Infof("certs dir is %s", dirname)

	cfg := certs.TLSConfig{
		CertPath:       path.Join(dirname, "service.crt"),
		KeyPath:        path.Join(dirname, "service.key"),
		RootCACertPath: path.Join(dirname, "root_ca.crt"),
	}

	serviceCerts, err := cfg.ReadCerts()
	if err != nil {
		log.WithError(err).Fatal("Could not load certs:")
	}

	return serviceCerts
}
