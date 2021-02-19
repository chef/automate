package integration_test

import (
	"context"
	"fmt"
	"os"
	"testing"

	"github.com/chef/automate/api/external/secrets"
	api "github.com/chef/automate/api/interservice/notifications/service"
	deploymentConst "github.com/chef/automate/components/automate-deployment/pkg/constants"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/certs"
)

var (
	suite = &Suite{}
	ctx   = context.Background()
)

// Suite holds any global state needed for the test run and implements
// GlobalSetup and GlobalTeardown methods.
type Suite struct {
	Client        api.NotificationsClient
	SecretsClient secrets.SecretsServiceClient
}

func (s *Suite) GlobalSetup() error {
	// Sometimes in local dev we do partial deployments with just a few services,
	// but the deployment-service should always be there. Using deployment's
	// certs should always work.
	deploymentCerts := certs.TLSConfig{
		CertPath:       deploymentConst.CertPath,
		KeyPath:        deploymentConst.KeyPath,
		RootCACertPath: deploymentConst.RootCertPath,
	}
	certData, err := deploymentCerts.ReadCerts()
	if err != nil {
		return err
	}

	mutTLSFactory := secureconn.NewFactory(*certData)
	notificationsEndpoint, err := mutTLSFactory.Dial("notifications-service", "localhost:10125")
	if err != nil {
		return err
	}

	s.Client = api.NewNotificationsClient(notificationsEndpoint)

	secretsEndpoint, err := mutTLSFactory.Dial("secrets-service", "localhost:10131")
	if err != nil {
		return err
	}
	s.SecretsClient = secrets.NewSecretsServiceClient(secretsEndpoint)

	return s.DeleteEverything()
}

func (s *Suite) GlobalTeardown() error {
	return s.DeleteEverything()
}

func (s *Suite) DeleteEverything() error {
	ruleList, err := s.Client.ListRules(ctx, &api.Empty{})
	if err != nil {
		return err
	}
	for _, r := range ruleList.Rules {
		_, err := s.Client.DeleteRule(ctx, &api.RuleIdentifier{Id: r.Id})
		if err != nil {
			return nil
		}
	}
	return nil
}

// TestMain allow us to run a setup before running our tests and also
// teardown everything after we have finished testing.
//
// (Check out 'suite_test.go')
//
// => Docs: https://golang.org/pkg/testing/#hdr-Main
func TestMain(m *testing.M) {
	// Global Setup hook: Here is where you can initialize anythings you need
	// for your tests to run
	err := suite.GlobalSetup()
	if err != nil {
		fmt.Println("Test suite failed during GlobalSetup")
		fmt.Println(err)
		os.Exit(1)
	}

	// Execute the test suite and record the exit code
	exitCode := m.Run()

	// Teardown hook: It says it all
	err = suite.GlobalTeardown()
	if err != nil {
		fmt.Println("Test suite failed during GlobalTeardown")
		fmt.Println(err)
		if exitCode != 0 {
			os.Exit(2)
		}
	}

	// call with result of m.Run()
	os.Exit(exitCode)
}
