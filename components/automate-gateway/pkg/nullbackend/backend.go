package nullbackend

// nullbackend is an aggregate Chef Automate backend gRPC server that implements
// failure responses for all requests. When the gateway is started without all
// required bind addresses we'll bind missing clients to the nullbackend.

import (
	"google.golang.org/grpc"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/api/external/data_feed"
	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/api/interservice/authn"
	"github.com/chef/automate/api/interservice/authz"
	authzv2 "github.com/chef/automate/api/interservice/authz/v2"
	cfgmgmt "github.com/chef/automate/api/interservice/cfgmgmt/service"
	"github.com/chef/automate/api/interservice/data_lifecycle"
	"github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/api/interservice/event_feed"
	"github.com/chef/automate/api/interservice/ingest"
	"github.com/chef/automate/api/interservice/license_control"

	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	teams "github.com/chef/automate/api/interservice/teams/v2"
	cc_jobs "github.com/chef/automate/components/compliance-service/api/jobs"
	cc_profiles "github.com/chef/automate/components/compliance-service/api/profiles"
	cc_reporting "github.com/chef/automate/components/compliance-service/api/reporting"
	cc_stats "github.com/chef/automate/components/compliance-service/api/stats"
	cc_version "github.com/chef/automate/components/compliance-service/api/version"
	cc_ingest "github.com/chef/automate/components/compliance-service/ingest/ingest"
	notifications "github.com/chef/automate/components/notifications-client/api"
)

// NewServer returns a pointer to a new instance of the null backend server
func NewServer() *grpc.Server {
	s := grpc.NewServer()

	applications.RegisterApplicationsServiceServer(s, &applications.UnimplementedApplicationsServiceServer{})
	authn.RegisterAuthenticationServer(s, &authn.UnimplementedAuthenticationServer{})
	authz.RegisterAuthorizationServer(s, &authz.UnimplementedAuthorizationServer{})
	authzv2.RegisterPoliciesServer(s, &authzv2.UnimplementedPoliciesServer{})
	cc_ingest.RegisterComplianceIngesterServer(s, &cc_ingest.UnimplementedComplianceIngesterServer{})
	cc_jobs.RegisterJobsServiceServer(s, &cc_jobs.UnimplementedJobsServiceServer{})
	cc_profiles.RegisterProfilesServiceServer(s, &cc_profiles.UnimplementedProfilesServiceServer{})
	cc_reporting.RegisterReportingServiceServer(s, &cc_reporting.UnimplementedReportingServiceServer{})
	cc_stats.RegisterStatsServiceServer(s, &cc_stats.UnimplementedStatsServiceServer{})
	cc_version.RegisterVersionServiceServer(s, &cc_version.UnimplementedVersionServiceServer{})
	cfgmgmt.RegisterCfgMgmtServer(s, &cfgmgmt.UnimplementedCfgMgmtServer{})
	data_feed.RegisterDatafeedServiceServer(s, &data_feed.UnimplementedDatafeedServiceServer{})
	data_lifecycle.RegisterPurgeServer(s, &data_lifecycle.UnimplementedPurgeServer{})
	deployment.RegisterDeploymentServer(s, &deployment.UnimplementedDeploymentServer{})
	event_feed.RegisterEventFeedServiceServer(s, &event_feed.UnimplementedEventFeedServiceServer{})
	ingest.RegisterEventHandlerServer(s, &ingest.UnimplementedEventHandlerServer{})
	ingest.RegisterChefIngesterServer(s, &ingest.UnimplementedChefIngesterServer{})
	ingest.RegisterJobSchedulerServer(s, &ingest.UnimplementedJobSchedulerServer{})
	license_control.RegisterLicenseControlServer(s, &license_control.UnimplementedLicenseControlServer{})
	manager.RegisterNodeManagerServiceServer(s, &manager.UnimplementedNodeManagerServiceServer{})
	nodes.RegisterNodesServiceServer(s, &nodes.UnimplementedNodesServiceServer{})
	notifications.RegisterNotificationsServer(s, &notifications.UnimplementedNotificationsServer{})
	teams.RegisterTeamsV2Server(s, &teams.UnimplementedTeamsV2Server{})
	secrets.RegisterSecretsServiceServer(s, &secrets.UnimplementedSecretsServiceServer{})

	return s
}
