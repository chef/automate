package gateway

import (
	"fmt"

	grpc_prometheus "github.com/grpc-ecosystem/go-grpc-prometheus"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/api/interservice/authn"
	authz "github.com/chef/automate/api/interservice/authz"
	iam_v2beta "github.com/chef/automate/api/interservice/authz/v2"
	cfgmgmt "github.com/chef/automate/api/interservice/cfgmgmt/service"
	deployment "github.com/chef/automate/api/interservice/deployment"
	event_feed_api "github.com/chef/automate/api/interservice/event_feed"
	chef_ingest "github.com/chef/automate/api/interservice/ingest"
	license_control "github.com/chef/automate/api/interservice/license_control"
	"github.com/chef/automate/api/interservice/local_user"
	teams_v1 "github.com/chef/automate/api/interservice/teams/v1"
	teams_v2 "github.com/chef/automate/api/interservice/teams/v2"
	jobs "github.com/chef/automate/components/compliance-service/api/jobs"
	profiles "github.com/chef/automate/components/compliance-service/api/profiles"
	cc_reporting "github.com/chef/automate/components/compliance-service/api/reporting"
	cc_stats "github.com/chef/automate/components/compliance-service/api/stats"
	cc_version "github.com/chef/automate/components/compliance-service/api/version"
	cc_ingest "github.com/chef/automate/components/compliance-service/ingest/ingest"
	manager "github.com/chef/automate/components/nodemanager-service/api/manager"
	nodes "github.com/chef/automate/components/nodemanager-service/api/nodes"
	notifications "github.com/chef/automate/components/notifications-client/api"
	"github.com/chef/automate/components/notifications-client/notifier"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tracing"
)

var defaultEndpoints = map[string]string{
	"authn-service":           "0.0.0.0:9091",
	"authz-service":           "0.0.0.0:10130",
	"config-mgmt-service":     "0.0.0.0:10119",
	"compliance-service":      "0.0.0.0:10121",
	"data-feed-service":       "0.0.0.0:14001",
	"deployment-service":      "0.0.0.0:10161",
	"ingest-service":          "0.0.0.0:10122",
	"license-control-service": "0.0.0.0:10124",
	"local-user-service":      "0.0.0.0:9092",
	"notifications-service":   "0.0.0.0:4001",
	"secrets-service":         "0.0.0.0:10131",
	"applications-service":    "0.0.0.0:10133",
	"nodemanager-service":     "0.0.0.0:10120",
	"event-feed-service":      "0.0.0.0:10134",
}

// clientMetrics holds the clients (identified by service) for which we'll
// enable gathering metrics through the grpc-middleware for prometheus. This
// comes at a cost, so we don't just enable all the clients.
// This list should include those clients that are not only serving a specific
// server endpoint, but are interesting to inspect for their own sake.
var clientMetrics = map[string]bool{
	"authn-service":  true, // this only exists as a client
	"authz-service":  true, // auth middleware calls are done using this client
	"ingest-service": true, // the handcrafted legacy ingestion handlers use this to bypass the server
}

// ClientConfig describes the endpoints we wish to connect to
type ClientConfig struct {
	Endpoints map[string]ConnectionOptions `mapstructure:"endpoints" toml:"endpoints"`
	Notifier  NotifierOptions              `mapstructure:"notifier" toml:"notifier"`
}

// ConnectionOptions describes how we wish to connect to a certain
// endpoint.
type ConnectionOptions struct {
	// Target to connect to
	Target string `mapstructure:"target" toml:"target"`
	Secure bool   `mapstructure:"secure" toml:"secure"`
}

// ClientsFactory is the interface for grpc client connections
type ClientsFactory interface {
	CfgMgmtClient() (cfgmgmt.CfgMgmtClient, error)
	IngestStatusClient() (chef_ingest.IngestStatusClient, error)
	ChefIngesterClient() (chef_ingest.ChefIngesterClient, error)
	ChefIngesterJobSchedulerClient() (chef_ingest.JobSchedulerClient, error)
	ComplianceIngesterClient() (cc_ingest.ComplianceIngesterClient, error)
	NotificationsClient() (notifications.NotificationsClient, error)
	AuthenticationClient() (authn.AuthenticationClient, error)
	AuthorizationClient() (authz.AuthorizationClient, error)
	AuthorizationV2Client() (iam_v2beta.AuthorizationClient, error)
	PoliciesClient() (iam_v2beta.PoliciesClient, error)
	ProjectsClient() (iam_v2beta.ProjectsClient, error)
	TeamsV1Client() (teams_v1.TeamsV1Client, error)
	TeamsV2Client() (teams_v2.TeamsV2Client, error)
	TokensMgmtClient() (authn.TokensMgmtClient, error)
	UsersMgmtClient() (local_user.UsersMgmtClient, error)
	Notifier() (notifier.Notifier, error)
	ApplicationsClient() (applications.ApplicationsServiceClient, error)
	SecretClient() (secrets.SecretsServiceClient, error)
	NodesClient() (nodes.NodesServiceClient, error)
	FeedClient() (event_feed_api.EventFeedServiceClient, error)
	ComplianceReportingServiceClient() (cc_reporting.ReportingServiceClient, error)
	ComplianceProfilesServiceClient() (profiles.ProfilesServiceClient, error)
	ComplianceJobsServiceClient() (jobs.JobsServiceClient, error)
	ComplianceStatsServiceClient() (cc_stats.StatsServiceClient, error)
	ComplianceVersionServiceClient() (cc_version.VersionServiceClient, error)
	NodeManagerClient() (manager.NodeManagerServiceClient, error)
	LicenseControlClient() (license_control.LicenseControlClient, error)
	DeploymentServiceClient() (deployment.DeploymentClient, error)
}

// clientsFactory caches grpc client connections and returns clients
type clientsFactory struct {
	config      ClientConfig
	connections map[string]*grpc.ClientConn

	// Notifications has a very heavy cost for us, therefore we
	// are going to have a unique client that we will initialize
	// when this object is created
	notifierClient notifier.Notifier
}

// NotifierOptions contains options used to configure Notifier
type NotifierOptions struct {
}

// NewClientsFactory creates a client factory that keeps one ClientConn per endpoint.
// Each factory method that returns a client will reuse the ClientConn endpoint.
// What this means is that you are allowed to cache each client. The target can
// never change. The underlying ClientConn will be responsible for maintaining
// the connection, where maintaining includes dealing with disconnects, service
// processes entering and leaving the pool, ip addresses changing, etc.
func NewClientsFactory(config ClientConfig, connFactory *secureconn.Factory) ClientsFactory {
	clients := clientsFactory{
		connections: map[string]*grpc.ClientConn{},
		config:      config,
	}

	// Note: this only affects selected clients, which receive the metrics
	// interceptor options -- see below.
	grpc_prometheus.EnableClientHandlingTimeHistogram()

	for service, endpoint := range config.Endpoints {
		if endpoint.Target != "" {
			metricsEnabled := clientMetrics[service]
			log.WithFields(log.Fields{
				"service":         service,
				"endpoint":        endpoint.Target,
				"metrics_enabled": metricsEnabled,
			}).Info("Dialing")

			var conn *grpc.ClientConn
			var err error

			opts := []grpc.DialOption{}
			if metricsEnabled {
				opts = append(opts,
					grpc.WithUnaryInterceptor(grpc_prometheus.UnaryClientInterceptor),
					grpc.WithStreamInterceptor(grpc_prometheus.StreamClientInterceptor))
			}

			if endpoint.Secure {
				conn, err = connFactory.Dial(service, endpoint.Target, opts...)
			} else {
				// Q: do we (still) have non-tls endpoints?
				conn, err = grpc.Dial(
					endpoint.Target,
					append(opts, grpc.WithInsecure(), tracing.GlobalClientInterceptor())...,
				)
			}

			if err != nil {
				// This case should never happen (unless you tell Dial to block)
				log.WithFields(log.Fields{
					"service": service,
					"error":   err,
				}).Fatal("Could not create connection")
			}
			clients.connections[service] = conn
		} else {
			log.WithFields(log.Fields{
				"service": service,
			}).Warn("Missing target")
		}
	}

	// Initialize the notifications client if exists
	n, exists := clients.connections["notifications-service"]
	if exists {
		clients.notifierClient = notifier.New(n)
	}

	return &clients
}

// CfgMgmtClient returns a client for the CfgMgmt service.
// It requires the `cfgmgmt` endpoint to be configured
func (c *clientsFactory) CfgMgmtClient() (cfgmgmt.CfgMgmtClient, error) {
	conn, err := c.connectionByName("config-mgmt-service")
	if err != nil {
		return nil, err
	}
	return cfgmgmt.NewCfgMgmtClient(conn), nil
}

// IngestStatusClient returns a client for the IngestStatus service.
// It requires the `ingest` endpoint to be configured
func (c *clientsFactory) IngestStatusClient() (chef_ingest.IngestStatusClient, error) {
	conn, err := c.connectionByName("ingest-service")
	if err != nil {
		return nil, err
	}
	return chef_ingest.NewIngestStatusClient(conn), nil
}

// ChefIngesterClient returns a client for the ChefIngester service.
// It requires the `ingest` endpoint to be configured
func (c *clientsFactory) ChefIngesterClient() (chef_ingest.ChefIngesterClient, error) {
	conn, err := c.connectionByName("ingest-service")
	if err != nil {
		return nil, err
	}
	return chef_ingest.NewChefIngesterClient(conn), nil
}

// ChefIngesterJobSchedulerClient returns a client for the ChefIngesterJobScheduler service.
// It requires the `ingest` endpoint to be configured
func (c *clientsFactory) ChefIngesterJobSchedulerClient() (chef_ingest.JobSchedulerClient, error) {
	conn, err := c.connectionByName("ingest-service")
	if err != nil {
		return nil, err
	}
	return chef_ingest.NewJobSchedulerClient(conn), nil
}

// ComplianceIngesterClient returns a client for the InspecIngester service.
// It requires the `ingest` endpoint to be configured
func (c *clientsFactory) ComplianceIngesterClient() (cc_ingest.ComplianceIngesterClient, error) {
	conn, err := c.connectionByName("compliance-service")
	if err != nil {
		return nil, err
	}
	return cc_ingest.NewComplianceIngesterClient(conn), nil
}

// NotificationsClient returns a client for the Notifications service.
// It requires the `notifications` endpoint to be configured
func (c *clientsFactory) NotificationsClient() (notifications.NotificationsClient, error) {
	conn, err := c.connectionByName("notifications-service")
	if err != nil {
		return nil, err
	}
	return notifications.NewNotificationsClient(conn), nil
}

// AuthenticationClient returns a client for the Authentication service.
// It requires the `auth` endpoint to be configured
func (c *clientsFactory) AuthenticationClient() (authn.AuthenticationClient, error) {
	conn, err := c.connectionByName("authn-service")
	if err != nil {
		return nil, err
	}
	return authn.NewAuthenticationClient(conn), nil
}

// AuthorizationClient returns a client for the Authorization service.
// It requires the `authz` endpoint to be configured
func (c *clientsFactory) AuthorizationClient() (authz.AuthorizationClient, error) {
	conn, err := c.connectionByName("authz-service")
	if err != nil {
		return nil, err
	}
	return authz.NewAuthorizationClient(conn), nil
}

// AuthorizationV2Client returns a client for the Authorization (IAMv2) service.
// It requires the `authz` endpoint to be configured
func (c *clientsFactory) AuthorizationV2Client() (iam_v2beta.AuthorizationClient, error) {
	conn, err := c.connectionByName("authz-service")
	if err != nil {
		return nil, err
	}
	return iam_v2beta.NewAuthorizationClient(conn), nil
}

// PoliciesClient returns a client for the Policies service.
// It requires the `authz` endpoint to be configured
func (c *clientsFactory) PoliciesClient() (iam_v2beta.PoliciesClient, error) {
	conn, err := c.connectionByName("authz-service")
	if err != nil {
		return nil, err
	}
	return iam_v2beta.NewPoliciesClient(conn), nil
}

// ProjectsClient returns a client for the Projects service.
// It requires the `authz` endpoint to be configured
func (c *clientsFactory) ProjectsClient() (iam_v2beta.ProjectsClient, error) {
	conn, err := c.connectionByName("authz-service")
	if err != nil {
		return nil, err
	}
	return iam_v2beta.NewProjectsClient(conn), nil
}

// TeamsV1Client returns a V1 client for the Teams service.
// It requires the `teams` endpoint to be configured
func (c *clientsFactory) TeamsV1Client() (teams_v1.TeamsV1Client, error) {
	conn, err := c.connectionByName("authz-service")
	if err != nil {
		return nil, err
	}
	return teams_v1.NewTeamsV1Client(conn), nil
}

// TeamsV2Client returns a V2 client for the Teams service.
// It requires the `teams` endpoint to be configured
func (c *clientsFactory) TeamsV2Client() (teams_v2.TeamsV2Client, error) {
	conn, err := c.connectionByName("authz-service")
	if err != nil {
		return nil, err
	}
	return teams_v2.NewTeamsV2Client(conn), nil
}

// TokensMgmtClient returns a client for the Tokens Mgmt service.
// It requires the `auth` endpoint to be configured
func (c *clientsFactory) TokensMgmtClient() (authn.TokensMgmtClient, error) {
	conn, err := c.connectionByName("authn-service")
	if err != nil {
		return nil, err
	}
	return authn.NewTokensMgmtClient(conn), nil
}

// UsersMgmtClient returns a client for the Users Mgmt service.
// It requires the `local-user-service` endpoint to be configured
func (c *clientsFactory) UsersMgmtClient() (local_user.UsersMgmtClient, error) {
	conn, err := c.connectionByName("local-user-service")
	if err != nil {
		return nil, err
	}
	return local_user.NewUsersMgmtClient(conn), nil
}

// Notifier returns the unique client for the Notifications service.
// We are reusing the same notifier since it's quite heavy
func (c *clientsFactory) Notifier() (notifier.Notifier, error) {
	var err error
	if c.notifierClient == nil {
		err = errors.New("Expected connection for notifications-service, but none was found. Check to make sure it is correctly configured")
	}
	return c.notifierClient, err
}

func (c *clientsFactory) ApplicationsClient() (applications.ApplicationsServiceClient, error) {
	conn, err := c.connectionByName("applications-service")
	if err != nil {
		return nil, err
	}
	return applications.NewApplicationsServiceClient(conn), nil
}

func (c *clientsFactory) SecretClient() (secrets.SecretsServiceClient, error) {
	conn, err := c.connectionByName("secrets-service")
	if err != nil {
		return nil, err
	}
	return secrets.NewSecretsServiceClient(conn), nil
}

func (c *clientsFactory) NodesClient() (nodes.NodesServiceClient, error) {
	conn, err := c.connectionByName("nodemanager-service")
	if err != nil {
		return nil, err
	}
	return nodes.NewNodesServiceClient(conn), nil
}

func (c *clientsFactory) FeedClient() (event_feed_api.EventFeedServiceClient, error) {
	conn, err := c.connectionByName("event-feed-service")
	if err != nil {
		return nil, err
	}
	return event_feed_api.NewEventFeedServiceClient(conn), nil
}

func (c *clientsFactory) ComplianceReportingServiceClient() (cc_reporting.ReportingServiceClient, error) {
	conn, err := c.connectionByName("compliance-service")
	if err != nil {
		return nil, err
	}
	return cc_reporting.NewReportingServiceClient(conn), nil
}

func (c *clientsFactory) ComplianceProfilesServiceClient() (profiles.ProfilesServiceClient, error) {
	conn, err := c.connectionByName("compliance-service")
	if err != nil {
		return nil, err
	}
	return profiles.NewProfilesServiceClient(conn), nil
}

func (c *clientsFactory) ComplianceJobsServiceClient() (jobs.JobsServiceClient, error) {
	conn, err := c.connectionByName("compliance-service")
	if err != nil {
		return nil, err
	}
	return jobs.NewJobsServiceClient(conn), nil
}

func (c *clientsFactory) ComplianceStatsServiceClient() (cc_stats.StatsServiceClient, error) {
	conn, err := c.connectionByName("compliance-service")
	if err != nil {
		return nil, err
	}
	return cc_stats.NewStatsServiceClient(conn), nil
}

func (c *clientsFactory) ComplianceVersionServiceClient() (cc_version.VersionServiceClient, error) {
	conn, err := c.connectionByName("compliance-service")
	if err != nil {
		return nil, err
	}
	return cc_version.NewVersionServiceClient(conn), nil
}

func (c *clientsFactory) NodeManagerClient() (manager.NodeManagerServiceClient, error) {
	conn, err := c.connectionByName("nodemanager-service")
	if err != nil {
		return nil, err
	}
	return manager.NewNodeManagerServiceClient(conn), nil
}

func (c *clientsFactory) LicenseControlClient() (license_control.LicenseControlClient, error) {
	conn, err := c.connectionByName("license-control-service")
	if err != nil {
		return nil, err
	}
	return license_control.NewLicenseControlClient(conn), nil
}

func (c *clientsFactory) DeploymentServiceClient() (deployment.DeploymentClient, error) {
	conn, err := c.connectionByName("deployment-service")
	if err != nil {
		return nil, err
	}
	return deployment.NewDeploymentClient(conn), nil
}

func (c *clientsFactory) connectionByName(name string) (*grpc.ClientConn, error) {
	conn, exists := c.connections[name]
	if !exists {
		err := fmt.Errorf("Expected connection for %s, but none was found. Check to make sure it is correctly configured", name)
		return nil, err
	}
	return conn, nil
}

// Populate our endpoints with default targets
func (c *ClientConfig) configureDefaultEndpoints() {
	if c.Endpoints == nil {
		c.Endpoints = map[string]ConnectionOptions{}
	}

	for service, target := range defaultEndpoints {
		// If the endpoint already exists don't overwrite it with a default
		if _, ok := c.Endpoints[service]; ok {
			continue
		}

		c.Endpoints[service] = ConnectionOptions{
			Target: target,
			Secure: true,
		}
	}
}
