package client

import (
	"github.com/chef/automate/components/automate-gateway/api/auth/teams"
	"github.com/chef/automate/components/automate-gateway/api/auth/tokens"
	"github.com/chef/automate/components/automate-gateway/api/auth/users"
	"github.com/chef/automate/components/automate-gateway/api/authz"
	"github.com/chef/automate/components/automate-gateway/api/compliance/reporting"
	"github.com/chef/automate/components/automate-gateway/api/iam/v2beta"
)

// APIClient is an API client ready for making requests against our public API.
// The client will auth via the deployment-service cert, which has a system level
// policy granting it universal access to our API.
type APIClient interface {
	// TODO (tc): Add other service clients here as needed.
	AuthzClient() authz.AuthorizationClient
	TeamsClient() teams.TeamsClient
	TeamsV2Client() v2beta.TeamsClient
	TokensClient() tokens.TokensMgmtClient
	TokensV2Client() v2beta.TokensClient
	UsersClient() users.UsersMgmtClient
	PoliciesClient() v2beta.PoliciesClient
	ReportingClient() reporting.ReportingServiceClient
	CloseConnection() error
}
