package client

import (
	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/components/automate-gateway/api/auth/teams"
	"github.com/chef/automate/components/automate-gateway/api/auth/users"
	"github.com/chef/automate/components/automate-gateway/api/authz"
	"github.com/chef/automate/components/automate-gateway/api/compliance/reporting"
)

// APIClient is an API client ready for making requests against our public API.
// The client will auth via the deployment-service cert, which has a system level
// policy granting it universal access to our API.
type APIClient interface {
	// TODO (tc): Add other service clients here as needed.
	AuthzClient() authz.AuthorizationClient
	TeamsClient() teams.TeamsClient
	TeamsV2Client() iam.TeamsClient
	TokensClient() iam.TokensClient
	UsersClient() users.UsersMgmtClient
	PoliciesClient() iam.PoliciesClient
	ReportingClient() reporting.ReportingServiceClient
	ApplicationsClient() applications.ApplicationsServiceClient
	CloseConnection() error
}
