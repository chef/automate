package integration_test

import (
	"os"
	"testing"

	"fmt"

	olivere "github.com/olivere/elastic"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/event-feed-service/pkg/persistence"
	"github.com/chef/automate/components/event-service/config"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/certs"
)

var (
	cfg         *config.EventConfig
	connFactory *secureconn.Factory

	// for integration testing with feed service
	esClient *olivere.Client
	indices  = []string{persistence.IndexNameFeeds}
	types    = []string{persistence.DocType}

	suite = NewSuite()
)

func doInit() {
	esURL := os.Getenv("ELASTICSEARCH_URL")
	client, err := olivere.NewClient(
		olivere.SetURL(esURL),
		olivere.SetSniff(false),
	)
	if err != nil {
		fmt.Printf("Could not create Elasticsearch client from '%s': %s\n", esURL, err)
		os.Exit(1)
	}

	esClient = client

	cfg = &config.EventConfig{
		ServiceConfig: config.ServiceConfig{
			Host:          "0.0.0.0",
			Port:          10132,
			EventLimit:    100000,
			ListenerLimit: 10000,
		},
		LogConfig: config.LogConfig{
			LogLevel:  "debug",
			LogFormat: "text",
		},
		TLSConfig: certs.TLSConfig{
			CertPath:       "/hab/svc/event-service/config/service.crt",
			KeyPath:        "/hab/svc/event-service/config/service.key",
			RootCACertPath: "/hab/svc/event-service/config/root_ca.crt",
		},
		HandlerEndpoints: config.HandlerConfig{
			Compliance: "0.0.0.0:10121",
			CfgIngest:  "0.0.0.0:10122",
			Authz:      "0.0.0.0:10130",
			EventFeed:  "0.0.0.0:10134",
		},
	}

	serviceCerts, err := cfg.TLSConfig.ReadCerts()
	if err != nil {
		log.WithFields(log.Fields{
			"err": err.Error(),
		}).Fatal("Failed to load x509 key pair and/or root CA certificate")
	}

	cfg.ServiceCerts = serviceCerts
	connFactory = secureconn.NewFactory(*cfg.ServiceCerts)
}

// TestMain allows us to set up a test fixture before the suite run
// and tear it down afterward.
//
// => Docs: https://golang.org/pkg/testing/#hdr-Main
func TestMain(m *testing.M) {
	// Global Setup hook: initialize anything required for the test
	// run, e.g., initializing ES indices, inserting test data, etc.
	suite.GlobalSetup()

	// Execute the test suite and record the exit code
	exitCode := m.Run()

	// Teardown hook: clean up any test fixtures created
	// for the test so that tests run independently
	suite.GlobalTeardown()

	// call with result of m.Run()
	os.Exit(exitCode)
}
