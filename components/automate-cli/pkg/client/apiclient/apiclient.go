package apiclient

import (
	"context"

	"google.golang.org/grpc"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/api/external/compliance/reporting"
	iam "github.com/chef/automate/api/external/iam/v2"
	client_type "github.com/chef/automate/components/automate-cli/pkg/client"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/constants"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/certs"
)

type client struct {
	apiClientConn *grpc.ClientConn
	// TODO (tc): Add other service clients here as needed.
	authzClient        iam.AuthorizationClient
	teamsClient        iam.TeamsClient
	tokensClient       iam.TokensClient
	usersClient        iam.UsersClient
	policiesClient     iam.PoliciesClient
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
		authzClient:        iam.NewAuthorizationClient(apiClientConn),
		teamsClient:        iam.NewTeamsClient(apiClientConn),
		tokensClient:       iam.NewTokensClient(apiClientConn),
		usersClient:        iam.NewUsersClient(apiClientConn),
		policiesClient:     iam.NewPoliciesClient(apiClientConn),
		reportingClient:    reporting.NewReportingServiceClient(apiClientConn),
		applicationsClient: applications.NewApplicationsServiceClient(apiClientConn),
	}, nil
}

func (c client) AuthzClient() iam.AuthorizationClient {
	return c.authzClient
}

func (c client) TeamsClient() iam.TeamsClient {
	return c.teamsClient
}

func (c client) TokensClient() iam.TokensClient {
	return c.tokensClient
}

func (c client) UsersClient() iam.UsersClient {
	return c.usersClient
}

func (c client) PoliciesClient() iam.PoliciesClient {
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
