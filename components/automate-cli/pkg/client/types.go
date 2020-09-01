package client

import (
	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/api/external/compliance/reporting"
	iam "github.com/chef/automate/api/external/iam/v2"
)

// APIClient is an API client ready for making requests against our public API.
// The client will auth via the deployment-service cert, which has a system level
// policy granting it universal access to our API.
type APIClient interface {
	AuthzClient() iam.AuthorizationClient
	TeamsClient() iam.TeamsClient
	TokensClient() iam.TokensClient
	UsersClient() iam.UsersClient
	PoliciesClient() iam.PoliciesClient
	ReportingClient() reporting.ReportingServiceClient
	ApplicationsClient() applications.ApplicationsServiceClient
	CloseConnection() error
}
