package integration_test

import (
	"fmt"
	"os"
	"testing"

	"google.golang.org/grpc"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/certs"
)

const (
	authZAddress = "localhost:10130"
	authNAddress = "localhost:10113"

	natsURLfmt = "nats://%s@localhost:4222"
)

var (
	suite = NewSuite()
)

type Suite struct {
	connFactory           *secureconn.Factory
	AuthPoliciesToRestore []*authz.Policy
	AuthzClient           authz.AuthorizationClient
	authzConn             *grpc.ClientConn
}

func NewSuite() *Suite {
	s := new(Suite)
	return s
}

func (s *Suite) GlobalSetup() error {
	if err := s.initConnFactory(); err != nil {
		return err
	}
	return s.initNATSAuthTest()
}

func (s *Suite) GlobalTeardown() {
	s.teardownNATSAuthTest()
}

func (s *Suite) initConnFactory() error {
	tlsConf := certs.TLSConfig{
		CertPath:       "/hab/svc/event-gateway/config/service.crt",
		KeyPath:        "/hab/svc/event-gateway/config/service.key",
		RootCACertPath: "/hab/svc/event-gateway/config/root_ca.crt",
	}

	certs, err := tlsConf.ReadCerts()
	if err != nil {
		return err
	}

	s.connFactory = secureconn.NewFactory(*certs)
	return nil
}

// TestMain allows us to set up a test fixture before the suite run
// and tear it down afterward.
//
// => Docs: https://golang.org/pkg/testing/#hdr-Main
func TestMain(m *testing.M) {
	// Global Setup hook: initialize anything required for the test
	// run, e.g., initializing ES indices, inserting test data, etc.
	if err := suite.GlobalSetup(); err != nil {
		fmt.Printf("Test failed global setup with err %v. Exiting.", err)
		os.Exit(1)
	}

	// Execute the test suite and record the exit code
	exitCode := m.Run()

	// Teardown hook: clean up any test fixtures created
	// for the test so that tests run independently
	suite.GlobalTeardown()

	// call with result of m.Run()
	os.Exit(exitCode)
}
