package mock

import (
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/components/automate-cli/pkg/client"
	"github.com/chef/automate/components/automate-gateway/api/auth/teams"
	"github.com/chef/automate/components/automate-gateway/api/auth/users"
	"github.com/chef/automate/components/automate-gateway/api/authz"
	"github.com/chef/automate/components/automate-gateway/api/compliance/reporting"
	iam "github.com/chef/automate/components/automate-gateway/api/iam/v2"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/test/helpers"
)

// Mock is a mocked out APIClient.
type Mock struct {
	authzClient        authz.AuthorizationClient
	teamsClient        teams.TeamsClient
	teamsV2Client      iam.TeamsClient
	TokensClient       iam.TokensClient
	usersClient        users.UsersMgmtClient
	policiesClient     iam.PoliciesClient
	reportingClient    reporting.ReportingServiceClient
	applicationsClient applications.ApplicationsServiceClient
	close              func()
}

// ServerMocks are mocked out API servers
type ServerMocks struct {
	AuthzMock    *authz.AuthorizationServerMock
	PoliciesMock *iam.PoliciesServerMock
	TeamsMock    *teams.TeamsServerMock
	TeamsV2Mock  *iam.TeamsServerMock
	TokensMock   *iam.TokensServerMock
	UsersMock    *users.UsersMgmtServerMock
}

// CreateMockConn returns a mocked version of APIClient that
// has clients that are connected to mock versions of the server.
func CreateMockConn(t *testing.T) (client.APIClient, ServerMocks, error) {
	t.Helper()
	deployCerts := helpers.LoadDevCerts(t, "automate-gateway")
	connFactory := secureconn.NewFactory(*deployCerts)
	grpcGateway := connFactory.NewServer()

	mockAuthz := authz.NewAuthorizationServerMock()
	authz.RegisterAuthorizationServer(grpcGateway, mockAuthz)

	mockTokens := iam.NewTokensServerMock()
	iam.RegisterTokensServer(grpcGateway, mockTokens)

	mockPolicies := iam.NewPoliciesServerMock()
	iam.RegisterPoliciesServer(grpcGateway, mockPolicies)

	mockTeams := teams.NewTeamsServerMock()
	teams.RegisterTeamsServer(grpcGateway, mockTeams)

	mockV2Teams := iam.NewTeamsServerMock()
	iam.RegisterTeamsServer(grpcGateway, mockV2Teams)

	mockUsers := users.NewUsersMgmtServerMock()
	users.RegisterUsersMgmtServer(grpcGateway, mockUsers)

	grpcServer := grpctest.NewServer(grpcGateway)
	gatewayConn, err := connFactory.Dial("automate-gateway", grpcServer.URL)
	require.NoError(t, err)

	return Mock{
			authzClient:        authz.NewAuthorizationClient(gatewayConn),
			teamsClient:        teams.NewTeamsClient(gatewayConn),
			teamsV2Client:      iam.NewTeamsClient(gatewayConn),
			TokensClient:       iam.NewTokensClient(gatewayConn),
			usersClient:        users.NewUsersMgmtClient(gatewayConn),
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
func (c Mock) AuthzClient() authz.AuthorizationClient {
	return c.authzClient
}

// TeamsClient returns mock TeamsClient
func (c Mock) TeamsClient() teams.TeamsClient {
	return c.teamsClient
}

// TeamsClient returns mock TeamsClient
func (c Mock) TeamsV2Client() iam.TeamsClient {
	return c.teamsV2Client
}

// TokensClient returns mock TokensClient
func (c Mock) TokensClient() iam.TokensClient {
	return c.TokensClient
}

// UsersClient returns mock UsersClient
func (c Mock) UsersClient() users.UsersMgmtClient {
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
