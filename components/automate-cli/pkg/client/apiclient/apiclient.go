package apiclient

import (
	"context"

	"google.golang.org/grpc"

	"github.com/chef/automate/api/external/applications"
	client_type "github.com/chef/automate/components/automate-cli/pkg/client"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/constants"
	"github.com/chef/automate/components/automate-gateway/api/auth/tokens"
	"github.com/chef/automate/components/automate-gateway/api/auth/users"
	"github.com/chef/automate/components/automate-gateway/api/authz"
	"github.com/chef/automate/components/automate-gateway/api/compliance/reporting"
	v2 "github.com/chef/automate/components/automate-gateway/api/iam/v2"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/certs"
)

type client struct {
	apiClientConn *grpc.ClientConn
	// TODO (tc): Add other service clients here as needed.
	authzClient        authz.AuthorizationClient
	teamsV2Client      v2.TeamsClient
	tokensClient       tokens.TokensMgmtClient
	tokensV2Client     v2.TokensClient
	usersClient        users.UsersMgmtClient
	policiesClient     v2.PoliciesClient
	reportingClient    reporting.ReportingServiceClient
	applicationsClient applications.ApplicationsServiceClient
}

// OpenConnection returns a new API client ready to make requests against our public API,
// These requests will auth via the deployment-service cert, which has a system level
// policy granting it universal access to our API.
func OpenConnection(ctx context.Context) (client_type.APIClient, error) {
	// TODO (tc): These values should come in via config and not be hard-coded.
	// Fixing this will be required for multi-node deploys where the cert might live somewhere else.
	deployCerts := certs.TLSConfig{
		CertPath:       constants.CertPath,
		KeyPath:        constants.KeyPath,
		RootCACertPath: constants.RootCertPath,
	}

	// TODO (tc): These values should come in via config and not be hard-coded.
	// Fixing this will be required for multi-node deploys. We can then go through the LB's
	// FQDN instead of the gateway directly.
	apiClientConn, err := newClientConn(ctx, deployCerts, "automate-gateway", "127.0.0.1:2001")
	if err != nil {
		return nil, err
	}

	return client{
		apiClientConn: apiClientConn,
		// TODO (tc): Add other service clients here as needed.
		authzClient:        authz.NewAuthorizationClient(apiClientConn),
		teamsV2Client:      v2.NewTeamsClient(apiClientConn),
		tokensClient:       tokens.NewTokensMgmtClient(apiClientConn),
		tokensV2Client:     v2.NewTokensClient(apiClientConn),
		usersClient:        users.NewUsersMgmtClient(apiClientConn),
		policiesClient:     v2.NewPoliciesClient(apiClientConn),
		reportingClient:    reporting.NewReportingServiceClient(apiClientConn),
		applicationsClient: applications.NewApplicationsServiceClient(apiClientConn),
	}, nil
}

func (c client) AuthzClient() authz.AuthorizationClient {
	return c.authzClient
}

func (c client) TeamsV2Client() v2.TeamsClient {
	return c.teamsV2Client
}

func (c client) TokensClient() tokens.TokensMgmtClient {
	return c.tokensClient
}

func (c client) TokensV2Client() v2.TokensClient {
	return c.tokensV2Client
}

func (c client) UsersClient() users.UsersMgmtClient {
	return c.usersClient
}

func (c client) PoliciesClient() v2.PoliciesClient {
	return c.policiesClient
}

func (c client) ReportingClient() reporting.ReportingServiceClient {
	return c.reportingClient
}

func (c client) ApplicationsClient() applications.ApplicationsServiceClient {
	return c.applicationsClient
}

// CloseConnection cleans up the temporary admin API token and closes all connections opened by client.
func (c client) CloseConnection() error {
	if err := c.apiClientConn.Close(); err != nil {
		return status.Wrap(err, status.APIError, "failed to close the connection to the Chef Automate API")
	}

	return nil
}

func newClientConn(ctx context.Context,
	connCerts certs.TLSConfig, serviceName, serviceAddress string) (*grpc.ClientConn, error) {

	serviceCerts, err := connCerts.ReadCerts()
	if err != nil {
		return nil, status.Wrap(err, status.FileAccessError, "failed to read tls keys/certs")
	}

	factory := secureconn.NewFactory(*serviceCerts)

	apiConn, err := factory.DialContext(
		ctx,
		serviceName,
		serviceAddress,
		grpc.WithBlock(),
	)

	if err != nil {
		return nil, status.Annotate(err, status.APIUnreachableError)
	}

	return apiConn, nil
}
