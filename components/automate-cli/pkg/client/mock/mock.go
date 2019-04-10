package mock

import (
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-cli/pkg/client"
	"github.com/chef/automate/components/automate-gateway/api/auth/teams"
	"github.com/chef/automate/components/automate-gateway/api/auth/tokens"
	"github.com/chef/automate/components/automate-gateway/api/auth/users"
	"github.com/chef/automate/components/automate-gateway/api/authz"
	"github.com/chef/automate/components/automate-gateway/api/compliance/reporting"
	"github.com/chef/automate/components/automate-gateway/api/iam/v2beta"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/test/helpers"
)

// Mock is a mocked out APIClient.
type Mock struct {
	authzClient     authz.AuthorizationClient
	teamsClient     teams.TeamsClient
	teamsV2Client   v2beta.TeamsClient
	tokensClient    tokens.TokensMgmtClient
	tokensV2Client  v2beta.TokensClient
	usersClient     users.UsersMgmtClient
	policiesClient  v2beta.PoliciesClient
	reportingClient reporting.ReportingServiceClient
	close           func()
}

// ServerMocks are mocked out API servers
type ServerMocks struct {
	AuthzMock    *authz.AuthorizationServerMock
	PoliciesMock *v2beta.PoliciesServerMock
	TeamsMock    *teams.TeamsServerMock
	TeamsV2Mock  *v2beta.TeamsServerMock
	TokensMock   *tokens.TokensMgmtServerMock
	TokensV2Mock *v2beta.TokensServerMock
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

	mockV2Tokens := v2beta.NewTokensServerMock()
	v2beta.RegisterTokensServer(grpcGateway, mockV2Tokens)

	mockPolicies := v2beta.NewPoliciesServerMock()
	v2beta.RegisterPoliciesServer(grpcGateway, mockPolicies)

	mockTeams := teams.NewTeamsServerMock()
	teams.RegisterTeamsServer(grpcGateway, mockTeams)

	mockV2Teams := v2beta.NewTeamsServerMock()
	v2beta.RegisterTeamsServer(grpcGateway, mockV2Teams)

	mockTokens := tokens.NewTokensMgmtServerMock()
	tokens.RegisterTokensMgmtServer(grpcGateway, mockTokens)

	mockUsers := users.NewUsersMgmtServerMock()
	users.RegisterUsersMgmtServer(grpcGateway, mockUsers)

	grpcServer := grpctest.NewServer(grpcGateway)
	gatewayConn, err := connFactory.Dial("automate-gateway", grpcServer.URL)
	require.NoError(t, err)

	return Mock{
			authzClient:     authz.NewAuthorizationClient(gatewayConn),
			teamsClient:     teams.NewTeamsClient(gatewayConn),
			teamsV2Client:   v2beta.NewTeamsClient(gatewayConn),
			tokensClient:    tokens.NewTokensMgmtClient(gatewayConn),
			tokensV2Client:  v2beta.NewTokensClient(gatewayConn),
			usersClient:     users.NewUsersMgmtClient(gatewayConn),
			policiesClient:  v2beta.NewPoliciesClient(gatewayConn),
			reportingClient: reporting.NewReportingServiceClient(gatewayConn),
			close:           grpcServer.Close,
		},
		ServerMocks{
			AuthzMock:    mockAuthz,
			PoliciesMock: mockPolicies,
			TeamsMock:    mockTeams,
			TokensMock:   mockTokens,
			TokensV2Mock: mockV2Tokens,
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
func (c Mock) TeamsV2Client() v2beta.TeamsClient {
	return c.teamsV2Client
}

// TokensClient returns mock TokensClient
func (c Mock) TokensClient() tokens.TokensMgmtClient {
	return c.tokensClient
}

// TokensV2Client returns mock TokensV2Client
func (c Mock) TokensV2Client() v2beta.TokensClient {
	return c.tokensV2Client
}

// UsersClient returns mock UsersClient
func (c Mock) UsersClient() users.UsersMgmtClient {
	return c.usersClient
}

// PoliciesClient returns mock PoliciesClient
func (c Mock) PoliciesClient() v2beta.PoliciesClient {
	return c.policiesClient
}

// ReportingClient returns mock ReportingClient
func (c Mock) ReportingClient() reporting.ReportingServiceClient {
	return c.reportingClient
}

// CloseConnection closes all connections opened by client
func (c Mock) CloseConnection() error {
	c.close()
	return nil
}
