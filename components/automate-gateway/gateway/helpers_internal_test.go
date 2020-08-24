// +build !mockgen

//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2018, Chef Software Inc.
//

package gateway

import (
	"context"
	"fmt"
	"io/ioutil"
	"testing"

	"github.com/golang/mock/gomock"

	"github.com/chef/automate/api/interservice/authn"
	"github.com/chef/automate/api/interservice/authz"
	compliance_ingest "github.com/chef/automate/api/interservice/compliance/ingest/ingest"
	ingest "github.com/chef/automate/api/interservice/ingest"
	middleware_authz "github.com/chef/automate/components/automate-gateway/gateway/middleware/authz"
	mock_gateway "github.com/chef/automate/components/automate-gateway/gateway_mocks/mock_gateway"
	"github.com/chef/automate/components/automate-gateway/pkg/limiter"
	"github.com/chef/automate/components/notifications-client/notifier"
)

var (
	rawruns     map[string][]byte
	rawactions  map[string][]byte
	rawliveness map[string][]byte
	rawreports  map[string][]byte
	mockURL     = "http://automate.example.com"
)

// newMockGatewayServer generates a mock gateway.Server instance, it receives
// a list of mocked services that will be injected to the ClientsFactory for its
// use when calling internal functions, handlers or even when passing the instance
// to a real object. Play with it!.
//
// Example: Create an Automate Gateway where the IngestClient is mocked
//
// func TestGatewayWithMockedIngestClient(t *testing.T) {
//   // Create a mocked Chef Ingest client
//   mockIngest := mock_ingest.NewMockChefIngesterClient(gomock.NewController(t))
//
//   // Assert that we will call the ProcessChefAction() func and return an error
//   mockIngest.EXPECT().ProcessChefAction(gomock.Any(), gomock.Any()).DoAndReturn(
//     func(_ context.Context, _ *ingestReq.Action) (*gp.Empty, error) {
//       return &gp.Empty{}, errors.New("Something happened")
//     },
//   )
//
//   // Create a new gateway.Server instance with mocked Clients
//   subject := newMockGatewayServer(t, mockIngest)
//
//   // Call functions, inspec, pass it to other objects. Play with it!
// }
func newMockGatewayServer(t *testing.T, services ...interface{}) Server {
	var (
		ctrl                    = gomock.NewController(t)
		mockClientsFactory      = mock_gateway.NewMockClientsFactory(ctrl)
		mockAuthorizationClient authz.AuthorizationServiceClient
		cfg                     = Config{}
	)

	// Add the provided mocked services
	for _, service := range services {
		switch s := service.(type) {
		case authn.AuthenticationServiceClient:
			// Mocking the provided mocked AuthorizationClient
			mockAuthenticationClient := authn.AuthenticationServiceClient(s)
			mockClientsFactory.EXPECT().AuthenticationClient().DoAndReturn(
				func() (authn.AuthenticationServiceClient, error) {
					return mockAuthenticationClient, nil
				},
			)
		case authz.AuthorizationServiceClient:
			// Mocking the provided mocked AuthorizationClient
			mockAuthorizationClient = authz.AuthorizationServiceClient(s)
			mockClientsFactory.EXPECT().AuthorizationClient().DoAndReturn(
				func() (authz.AuthorizationServiceClient, error) {
					return mockAuthorizationClient, nil
				},
			)
		case compliance_ingest.ComplianceIngesterServiceClient:
			// Mocking the provided mocked ComplianceIngesterClient
			mockComplianceIngester := compliance_ingest.ComplianceIngesterServiceClient(s)
			mockClientsFactory.EXPECT().ComplianceIngesterClient().DoAndReturn(
				func() (compliance_ingest.ComplianceIngesterServiceClient, error) {
					return mockComplianceIngester, nil
				},
			)
		case notifier.Notifier:
			// Mocking the provided mocked Notifier
			mockNotifier := notifier.Notifier(s)
			mockClientsFactory.EXPECT().Notifier().DoAndReturn(
				func() (notifier.Notifier, error) {
					return mockNotifier, nil
				},
			)
		case ingest.ChefIngesterServiceClient:
			mockIngestClient := ingest.ChefIngesterServiceClient(s)
			mockClientsFactory.EXPECT().ChefIngesterClient().DoAndReturn(
				func() (ingest.ChefIngesterServiceClient, error) {
					return mockIngestClient, nil
				},
			)
		default:
			panic("Service not implemented!")
		}
	}

	// Mock the AutomateURL
	cfg.ExternalFqdn = mockURL

	gw := New(cfg)
	gw.clientsFactory = mockClientsFactory
	gw.authorizer = middleware_authz.AuthorizationHandler(mockAuthorizationClient)
	gw.dataCollectorLimiter = limiter.NewNoopRequestLimiter()

	return *gw
}

// newAuthorizationMocks generates new mocks for AuthN and AuthZ to authorize
// a single resource and action
func newAuthorizationMocks(t *testing.T, resource, action string) (
	authn.AuthenticationServiceClient, authz.AuthorizationServiceClient) {
	var (
		ctrl            = gomock.NewController(t)
		mockAuthClient  = authn.NewMockAuthenticationServiceClient(ctrl)
		mockAuthzClient = authz.NewMockAuthorizationServiceClient(ctrl)
	)

	// Mocking AuthN Calls
	mockAuthClient.EXPECT().Authenticate(gomock.Any(), gomock.Any()).DoAndReturn(
		func(_ context.Context, _ *authn.AuthenticateRequest) (*authn.AuthenticateResponse, error) {
			return &authn.AuthenticateResponse{Subject: "mock", Teams: []string{}}, nil
		})

	// Mocking AuthZ Calls
	mockAuthzClient.EXPECT().ProjectsAuthorized(
		gomock.Any(),
		&authz.ProjectsAuthorizedReq{
			Subjects:       []string{"mock"},
			Resource:       resource,
			Action:         action,
			ProjectsFilter: []string{},
		},
	).DoAndReturn(
		func(_ context.Context, _ *authz.ProjectsAuthorizedReq) (*authz.ProjectsAuthorizedResp, error) {
			return &authz.ProjectsAuthorizedResp{Projects: []string{"any"}}, nil
		},
	)

	return mockAuthClient, mockAuthzClient
}

// loadRawExamples will load a set of examples that are stored inside
// this repository and make them available for testing
func loadRawExamples() {
	// load chef_run's for reuse
	rawruns = make(map[string][]byte)
	runs := []string{
		"../../ingest-service/examples/chef_client_run.json",
		"../../ingest-service/examples/converge-bad-report.json",
		"../../ingest-service/examples/converge-failure-report.json",
		"../../ingest-service/examples/converge-success-report.json",
	}
	for _, r := range runs {
		// load chef_run json into memory, so that we do not count the json generation
		content, err := ioutil.ReadFile(r)
		if err != nil {
			panic(err)
		}
		rawruns[r] = content
	}

	// load chef_action's for reuse
	rawactions = make(map[string][]byte)
	actions := []string{
		"bag_create",
		"bag_create",
		"bag_delete",
		"client_create",
		"cookbookartifactversion_update",
		"environment_create",
		"environment_delete",
		"environment_update",
		"group_create",
		"group_update",
		"item_bag_create",
		"item_bag_update",
		"node_create",
		"node_delete",
		"org_create",
		"permission_update_container",
		"permission_update_cookbook",
		"permission_update_environment",
		"policy_update",
		"user_associate",
		"user_create",
		"user_invite",
		"user_update",
		"version_cookbook_create",
		"version_cookbook_update",
	}
	for _, a := range actions {
		content, err := ioutil.ReadFile(fmt.Sprintf("../../ingest-service/examples/actions/%s.json", a))
		if err != nil {
			panic(err)
		}
		rawactions[a] = content
	}

	// load liveness's for reuse
	rawliveness = make(map[string][]byte)
	liveness := []string{
		"liveness_ping",
	}
	for _, l := range liveness {
		content, err := ioutil.ReadFile(fmt.Sprintf("../../ingest-service/examples/%s.json", l))
		if err != nil {
			panic(err)
		}
		rawliveness[l] = content
	}

	// load report's for reuse
	rawreports = make(map[string][]byte)
	reports := []string{
		"compliance-failure-big-report",
		"compliance-success-tiny-report",
	}
	for _, r := range reports {
		content, err := ioutil.ReadFile(fmt.Sprintf("../../compliance-service/ingest/examples/%s.json", r))
		if err != nil {
			panic(err)
		}
		rawreports[r] = content
	}
}
