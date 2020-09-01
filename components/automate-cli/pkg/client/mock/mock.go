package mock

import (
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/api/external/compliance/reporting"
	iam "github.com/chef/automate/api/external/iam/v2"
	"github.com/chef/automate/components/automate-cli/pkg/client"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/test/helpers"
)

// Mock is a mocked out APIClient.
type Mock struct {
	authzClient        iam.AuthorizationClient
	teamsClient        iam.TeamsClient
	tokensClient       iam.TokensClient
	usersClient        iam.UsersClient
	policiesClient     iam.PoliciesClient
	reportingClient    reporting.ReportingServiceClient
	applicationsClient applications.ApplicationsServiceClient
	close              func()
}

// ServerMocks are mocked out API servers
type ServerMocks struct {
	AuthzMock    *iam.AuthorizationServerMock
	PoliciesMock *iam.PoliciesServerMock
	TeamsMock    *iam.TeamsServerMock
	TokensMock   *iam.TokensServerMock
	UsersMock    *iam.UsersServerMock
}

// CreateMockConn returns a mocked version of APIClient that
// has clients that are connected to mock versions of the server.
func CreateMockConn(t *testing.T) (client.APIClient, ServerMocks, error) {
	t.Helper()
	deployCerts := helpers.LoadDevCerts(t, "automate-gateway")
	connFactory := secureconn.NewFactory(*deployCerts)
	grpcGateway := connFactory.NewServer()

	mockAuthz := iam.NewAuthorizationServerMock()
	iam.RegisterAuthorizationServer(grpcGateway, mockAuthz)

	mockTokens := iam.NewTokensServerMock()
	iam.RegisterTokensServer(grpcGateway, mockTokens)

	mockPolicies := iam.NewPoliciesServerMock()
	iam.RegisterPoliciesServer(grpcGateway, mockPolicies)

	mockTeams := iam.NewTeamsServerMock()
	iam.RegisterTeamsServer(grpcGateway, mockTeams)

	mockUsers := iam.NewUsersServerMock()
	iam.RegisterUsersServer(grpcGateway, mockUsers)

	grpcServer := grpctest.NewServer(grpcGateway)
	gatewayConn, err := connFactory.Dial("automate-gateway", grpcServer.URL)
	require.NoError(t, err)

	return Mock{
			authzClient:        iam.NewAuthorizationClient(gatewayConn),
			teamsClient:        iam.NewTeamsClient(gatewayConn),
			tokensClient:       iam.NewTokensClient(gatewayConn),
			usersClient:        iam.NewUsersClient(gatewayConn),
			policiesClient:     iam.NewPoliciesClient(gatewayConn),
			reportingClient:    reporting.NewReportingServiceClient(gatewayConn),
			applicationsClient: applications.NewApplicationsServiceClient(gatewayConn),
			close:              grpcServer.Close,
		},
		ServerMocks{
			AuthzMock:    mockAuthz,
			PoliciesMock: mockPolicies,
			TeamsMock:    mockTeams,
			TokensMock:   mockTokens,
			UsersMock:    mockUsers,
		},
		nil
}

// AuthzClient returns mock AuthzClient
func (c Mock) AuthzClient() iam.AuthorizationClient {
	return c.authzClient
}

// TeamsClient returns mock TeamsClient
func (c Mock) TeamsClient() iam.TeamsClient {
	return c.teamsClient
}

// TokensClient returns mock TokensClient
func (c Mock) TokensClient() iam.TokensClient {
	return c.tokensClient
}

// UsersClient returns mock UsersClient
func (c Mock) UsersClient() iam.UsersClient {
	return c.usersClient
}

// PoliciesClient returns mock PoliciesClient
func (c Mock) PoliciesClient() iam.PoliciesClient {
	return c.policiesClient
}

// ReportingClient returns mock ReportingClient
func (c Mock) ReportingClient() reporting.ReportingServiceClient {
	return c.reportingClient
}

// ApplicationsClient returns mock ApplicationsClient
func (c Mock) ApplicationsClient() applications.ApplicationsServiceClient {
	return c.applicationsClient
}

// CloseConnection closes all connections opened by client
func (c Mock) CloseConnection() error {
	c.close()
	return nil
}
