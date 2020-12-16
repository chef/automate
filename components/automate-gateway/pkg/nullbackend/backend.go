package nullbackend

// nullbackend is an aggregate Chef Automate backend gRPC server that implements
// failure responses for all requests. When the gateway is started without all
// required bind addresses we'll bind missing clients to the nullbackend.

import (
	"google.golang.org/grpc"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/api/external/cds"
	"github.com/chef/automate/api/external/data_feed"
	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/api/interservice/authn"
	"github.com/chef/automate/api/interservice/authz"
	cfgmgmt "github.com/chef/automate/api/interservice/cfgmgmt/service"
	"github.com/chef/automate/api/interservice/data_lifecycle"
	"github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/api/interservice/event_feed"
	"github.com/chef/automate/api/interservice/ingest"
	"github.com/chef/automate/api/interservice/license_control"

	cc_ingest "github.com/chef/automate/api/interservice/compliance/ingest/ingest"
	cc_jobs "github.com/chef/automate/api/interservice/compliance/jobs"
	cc_profiles "github.com/chef/automate/api/interservice/compliance/profiles"
	cc_reporting "github.com/chef/automate/api/interservice/compliance/reporting"
	cc_stats "github.com/chef/automate/api/interservice/compliance/stats"
	cc_version "github.com/chef/automate/api/interservice/compliance/version"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	"github.com/chef/automate/api/interservice/teams"
	notifications "github.com/chef/automate/components/notifications-client/api"
)

// NewServer returns a pointer to a new instance of the null backend server
func NewServer() *grpc.Server {
	// The nullbackend only listens on a unix socket. And it doesn't deal with interesting data,
	// but only "no implemented" responses.
	s := grpc.NewServer() // nosemgrep: go.grpc.security.grpc-server-insecure-connection.grpc-server-insecure-connection

	applications.RegisterApplicationsServiceServer(s, &applications.UnimplementedApplicationsServiceServer{})
	cds.RegisterCdsServer(s, &cds.UnimplementedCdsServer{})
	authn.RegisterAuthenticationServiceServer(s, &authn.UnimplementedAuthenticationServiceServer{})
	authz.RegisterPoliciesServiceServer(s, &authz.UnimplementedPoliciesServiceServer{})
	cc_ingest.RegisterComplianceIngesterServiceServer(s, &cc_ingest.UnimplementedComplianceIngesterServiceServer{})
	cc_jobs.RegisterJobsServiceServer(s, &cc_jobs.UnimplementedJobsServiceServer{})
	cc_profiles.RegisterProfilesServiceServer(s, &cc_profiles.UnimplementedProfilesServiceServer{})
	cc_reporting.RegisterReportingServiceServer(s, &cc_reporting.UnimplementedReportingServiceServer{})
	cc_stats.RegisterStatsServiceServer(s, &cc_stats.UnimplementedStatsServiceServer{})
	cc_version.RegisterVersionServiceServer(s, &cc_version.UnimplementedVersionServiceServer{})
	cfgmgmt.RegisterCfgMgmtServiceServer(s, &cfgmgmt.UnimplementedCfgMgmtServiceServer{})
	data_feed.RegisterDatafeedServiceServer(s, &data_feed.UnimplementedDatafeedServiceServer{})
	data_lifecycle.RegisterPurgeServer(s, &data_lifecycle.UnimplementedPurgeServer{})
	deployment.RegisterDeploymentServer(s, &deployment.UnimplementedDeploymentServer{})
	event_feed.RegisterEventFeedServiceServer(s, &event_feed.UnimplementedEventFeedServiceServer{})
	ingest.RegisterEventHandlerServiceServer(s, &ingest.UnimplementedEventHandlerServiceServer{})
	ingest.RegisterChefIngesterServiceServer(s, &ingest.UnimplementedChefIngesterServiceServer{})
	ingest.RegisterJobSchedulerServiceServer(s, &ingest.UnimplementedJobSchedulerServiceServer{})
	license_control.RegisterLicenseControlServiceServer(s, &license_control.UnimplementedLicenseControlServiceServer{})
	manager.RegisterNodeManagerServiceServer(s, &manager.UnimplementedNodeManagerServiceServer{})
	nodes.RegisterNodesServiceServer(s, &nodes.UnimplementedNodesServiceServer{})
	notifications.RegisterNotificationsServer(s, &notifications.UnimplementedNotificationsServer{})
	teams.RegisterTeamsServiceServer(s, &teams.UnimplementedTeamsServiceServer{})
	secrets.RegisterSecretsServiceServer(s, &secrets.UnimplementedSecretsServiceServer{})

	return s
}
