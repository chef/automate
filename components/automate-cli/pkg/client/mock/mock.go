package mock

import (
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/components/automate-cli/pkg/client"
	"github.com/chef/automate/components/automate-gateway/api/auth/tokens"
	"github.com/chef/automate/components/automate-gateway/api/auth/users"
	"github.com/chef/automate/components/automate-gateway/api/authz"
	"github.com/chef/automate/components/automate-gateway/api/compliance/reporting"
	v2 "github.com/chef/automate/components/automate-gateway/api/iam/v2"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/test/helpers"
)

// Mock is a mocked out APIClient.
type Mock struct {
	authzClient        authz.AuthorizationClient
	teamsV2Client      v2.TeamsClient
	tokensClient       tokens.TokensMgmtClient
	tokensV2Client     v2.TokensClient
	usersClient        users.UsersMgmtClient
	policiesClient     v2.PoliciesClient
	reportingClient    reporting.ReportingServiceClient
	applicationsClient applications.ApplicationsServiceClient
	close              func()
}

// ServerMocks are mocked out API servers
type ServerMocks struct {
	AuthzMock    *authz.AuthorizationServerMock
	PoliciesMock *v2.PoliciesServerMock
	TeamsV2Mock  *v2.TeamsServerMock
	TokensMock   *tokens.TokensMgmtServerMock
	TokensV2Mock *v2.TokensServerMock
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

	mockV2Tokens := v2.NewTokensServerMock()
	v2.RegisterTokensServer(grpcGateway, mockV2Tokens)

	mockPolicies := v2.NewPoliciesServerMock()
	v2.RegisterPoliciesServer(grpcGateway, mockPolicies)

	mockV2Teams := v2.NewTeamsServerMock()
	v2.RegisterTeamsServer(grpcGateway, mockV2Teams)

	mockTokens := tokens.NewTokensMgmtServerMock()
	tokens.RegisterTokensMgmtServer(grpcGateway, mockTokens)

	mockUsers := users.NewUsersMgmtServerMock()
	users.RegisterUsersMgmtServer(grpcGateway, mockUsers)

	grpcServer := grpctest.NewServer(grpcGateway)
	gatewayConn, err := connFactory.Dial("automate-gateway", grpcServer.URL)
	require.NoError(t, err)

	return Mock{
			authzClient:        authz.NewAuthorizationClient(gatewayConn),
			teamsV2Client:      v2.NewTeamsClient(gatewayConn),
			tokensClient:       tokens.NewTokensMgmtClient(gatewayConn),
			tokensV2Client:     v2.NewTokensClient(gatewayConn),
			usersClient:        users.NewUsersMgmtClient(gatewayConn),
			policiesClient:     v2.NewPoliciesClient(gatewayConn),
			reportingClient:    reporting.NewReportingServiceClient(gatewayConn),
			applicationsClient: applications.NewApplicationsServiceClient(gatewayConn),
			close:              grpcServer.Close,
		},
		ServerMocks{
			AuthzMock:    mockAuthz,
			PoliciesMock: mockPolicies,
			TeamsV2Mock:  mockV2Teams,
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
func (c Mock) TeamsV2Client() v2.TeamsClient {
	return c.teamsV2Client
}

// TokensClient returns mock TokensClient
func (c Mock) TokensClient() tokens.TokensMgmtClient {
	return c.tokensClient
}

// TokensV2Client returns mock TokensV2Client
func (c Mock) TokensV2Client() v2.TokensClient {
	return c.tokensV2Client
}

// UsersClient returns mock UsersClient
func (c Mock) UsersClient() users.UsersMgmtClient {
	return c.usersClient
}

// PoliciesClient returns mock PoliciesClient
func (c Mock) PoliciesClient() v2.PoliciesClient {
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
