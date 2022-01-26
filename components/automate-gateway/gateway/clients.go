package gateway

import (
	"fmt"
	"github.com/chef/automate/api/interservice/user_settings"

	grpc_prometheus "github.com/grpc-ecosystem/go-grpc-prometheus"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/api/external/data_feed"
	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/api/interservice/authn"
	"github.com/chef/automate/api/interservice/authz"
	cds "github.com/chef/automate/api/interservice/cds/service"
	cfgmgmt "github.com/chef/automate/api/interservice/cfgmgmt/service"
	cc_ingest "github.com/chef/automate/api/interservice/compliance/ingest/ingest"
	"github.com/chef/automate/api/interservice/compliance/jobs"
	profiles "github.com/chef/automate/api/interservice/compliance/profiles"
	cc_reporting "github.com/chef/automate/api/interservice/compliance/reporting"
	cc_stats "github.com/chef/automate/api/interservice/compliance/stats"
	cc_version "github.com/chef/automate/api/interservice/compliance/version"
	"github.com/chef/automate/api/interservice/data_lifecycle"
	"github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/api/interservice/event_feed"
	infra_proxy_migration "github.com/chef/automate/api/interservice/infra_proxy/migrations/service"
	infra_proxy "github.com/chef/automate/api/interservice/infra_proxy/service"
	chef_ingest "github.com/chef/automate/api/interservice/ingest"
	"github.com/chef/automate/api/interservice/license_control"
	"github.com/chef/automate/api/interservice/local_user"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	"github.com/chef/automate/api/interservice/teams"
	notifications "github.com/chef/automate/components/notifications-client/api"
	"github.com/chef/automate/components/notifications-client/notifier"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tracing"
)

var ErrNoConnectionConfigured = errors.New("Client not configured")

// grpcServices are gRPC backend services that we'll connect to to build clients
var grpcServices = []string{
	"applications-service",
	"authn-service",
	"authz-service",
	"automate-cds",
	"compliance-service",
	"config-mgmt-service",
	"data-feed-service",
	"deployment-service",
	"event-feed-service",
	"infra-proxy-service",
	"ingest-service",
	"license-control-service",
	"local-user-service",
	"nodemanager-service",
	"notifications-service",
	"teams-service",
	"secrets-service",
	"user-settings-service",
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
	Endpoints       map[string]ConnectionOptions `mapstructure:"endpoints" toml:"endpoints"`
	Notifier        NotifierOptions              `mapstructure:"notifier" toml:"notifier"`
	NullBackendSock string                       `mapstructure:"null_backend_sock" toml:"null_backend_sock"`
}

type ClientConnections map[string]*grpc.ClientConn

// ConnectionOptions describes how we wish to connect to a certain
// endpoint.
type ConnectionOptions struct {
	// Target to connect to
	Target string `mapstructure:"target" toml:"target"`
	Secure bool   `mapstructure:"secure" toml:"secure"`
}

// ClientsFactory is the interface for grpc client connections
type ClientsFactory interface {
	CfgMgmtClient() (cfgmgmt.CfgMgmtServiceClient, error)
	IngestStatusClient() (chef_ingest.IngestStatusServiceClient, error)
	ChefIngesterClient() (chef_ingest.ChefIngesterServiceClient, error)
	ChefIngesterJobSchedulerClient() (chef_ingest.JobSchedulerServiceClient, error)
	ComplianceIngesterClient() (cc_ingest.ComplianceIngesterServiceClient, error)
	NotificationsClient() (notifications.NotificationsClient, error)
	AuthenticationClient() (authn.AuthenticationServiceClient, error)
	AuthorizationClient() (authz.AuthorizationServiceClient, error)
	PoliciesClient() (authz.PoliciesServiceClient, error)
	ProjectsClient() (authz.ProjectsServiceClient, error)
	TeamsClient() (teams.TeamsServiceClient, error)
	TokensMgmtClient() (authn.TokensMgmtServiceClient, error)
	UsersMgmtClient() (local_user.UsersMgmtServiceClient, error)
	Notifier() (notifier.Notifier, error)
	ApplicationsClient() (applications.ApplicationsServiceClient, error)
	SecretClient() (secrets.SecretsServiceClient, error)
	NodesClient() (nodes.NodesServiceClient, error)
	FeedClient() (event_feed.EventFeedServiceClient, error)
	ComplianceReportingServiceClient() (cc_reporting.ReportingServiceClient, error)
	ComplianceProfilesServiceClient() (profiles.ProfilesServiceClient, error)
	ComplianceJobsServiceClient() (jobs.JobsServiceClient, error)
	ComplianceStatsServiceClient() (cc_stats.StatsServiceClient, error)
	ComplianceVersionServiceClient() (cc_version.VersionServiceClient, error)
	NodeManagerClient() (manager.NodeManagerServiceClient, error)
	LicenseControlClient() (license_control.LicenseControlServiceClient, error)
	DeploymentServiceClient() (deployment.DeploymentClient, error)
	DatafeedClient() (data_feed.DatafeedServiceClient, error)
	PurgeClient(service string) (data_lifecycle.PurgeClient, error)
	InfraProxyClient() (infra_proxy.InfraProxyServiceClient, error)
	InfraProxyMigrationClient() (infra_proxy_migration.MigrationDataServiceClient, error)
	CdsClient() (cds.AutomateCdsServiceClient, error)
	UserSettingsClient() (user_settings.UserSettingsServiceClient, error)
	Close() error
}

// clientsFactory caches grpc client connections and returns clients
type clientsFactory struct {
	config      ClientConfig
	connections ClientConnections

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
//
// In cases where our configuration is missing known endpoints and a null backend
// socket location has been set in the ClientConfig, we'll connect all missing
// clients to the null backend via the socket. The null backend is an aggregate server
// backend that implements failures for all backend gRPC's that the gateway needs.
// This is useful because depending on the products that are deployed, not all backend
// services will be guaranteed to running and therefore the target bind information
// for those services won't be present. Yet we still want to the gateway to
// start up up successfully, to initialize clients, and to properly
// return errors when backend services are not up. Without this, gRPC will
// continue to dial non-existent service targets and spam the logs endlessly.
func NewClientsFactory(config ClientConfig, connFactory *secureconn.Factory) (ClientsFactory, error) {
	var err error

	// Note: this only affects selected clients, which receive the metrics
	// interceptor options -- see below.
	grpc_prometheus.EnableClientHandlingTimeHistogram()

	clients := &clientsFactory{config: config}
	clients.connections, err = config.DialEndpoints(connFactory)
	if err != nil {
		return clients, err
	}

	// Initialize the notifications client if exists
	n, exists := clients.connections["notifications-service"]
	if exists {
		clients.notifierClient = notifier.New(n)
	}

	return clients, nil
}

// CfgMgmtClient returns a client for the CfgMgmt service.
// It requires the `cfgmgmt` endpoint to be configured
func (c *clientsFactory) CfgMgmtClient() (cfgmgmt.CfgMgmtServiceClient, error) {
	conn, err := c.connectionByName("config-mgmt-service")
	if err != nil {
		return nil, err
	}
	return cfgmgmt.NewCfgMgmtServiceClient(conn), nil
}

// CdsClient returns a client for the automate-cds service.
// It requires the `automate-cds` endpoint to be configured
func (c *clientsFactory) CdsClient() (cds.AutomateCdsServiceClient, error) {
	conn, err := c.connectionByName("automate-cds")
	if err != nil {
		return nil, err
	}
	return cds.NewAutomateCdsServiceClient(conn), nil
}

// IngestStatusClient returns a client for the IngestStatus service.
// It requires the `ingest` endpoint to be configured
func (c *clientsFactory) IngestStatusClient() (chef_ingest.IngestStatusServiceClient, error) {
	conn, err := c.connectionByName("ingest-service")
	if err != nil {
		return nil, err
	}
	return chef_ingest.NewIngestStatusServiceClient(conn), nil
}

// ChefIngesterClient returns a client for the ChefIngester service.
// It requires the `ingest` endpoint to be configured
func (c *clientsFactory) ChefIngesterClient() (chef_ingest.ChefIngesterServiceClient, error) {
	conn, err := c.connectionByName("ingest-service")
	if err != nil {
		return nil, err
	}
	return chef_ingest.NewChefIngesterServiceClient(conn), nil
}

// ChefIngesterJobSchedulerClient returns a client for the ChefIngesterJobScheduler service.
// It requires the `ingest` endpoint to be configured
func (c *clientsFactory) ChefIngesterJobSchedulerClient() (chef_ingest.JobSchedulerServiceClient, error) {
	conn, err := c.connectionByName("ingest-service")
	if err != nil {
		return nil, err
	}
	return chef_ingest.NewJobSchedulerServiceClient(conn), nil
}

// ComplianceIngesterClient returns a client for the InspecIngester service.
// It requires the `ingest` endpoint to be configured
func (c *clientsFactory) ComplianceIngesterClient() (cc_ingest.ComplianceIngesterServiceClient, error) {
	conn, err := c.connectionByName("compliance-service")
	if err != nil {
		return nil, err
	}
	return cc_ingest.NewComplianceIngesterServiceClient(conn), nil
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
func (c *clientsFactory) AuthenticationClient() (authn.AuthenticationServiceClient, error) {
	conn, err := c.connectionByName("authn-service")
	if err != nil {
		return nil, err
	}
	return authn.NewAuthenticationServiceClient(conn), nil
}

// AuthorizationClient returns a client for the Authorization service.
// It requires the `authz` endpoint to be configured
func (c *clientsFactory) AuthorizationClient() (authz.AuthorizationServiceClient, error) {
	conn, err := c.connectionByName("authz-service")
	if err != nil {
		return nil, err
	}
	return authz.NewAuthorizationServiceClient(conn), nil
}

// PoliciesClient returns a client for the Policies service.
// It requires the `authz` endpoint to be configured
func (c *clientsFactory) PoliciesClient() (authz.PoliciesServiceClient, error) {
	conn, err := c.connectionByName("authz-service")
	if err != nil {
		return nil, err
	}
	return authz.NewPoliciesServiceClient(conn), nil
}

// ProjectsClient returns a client for the Projects service.
// It requires the `authz` endpoint to be configured
func (c *clientsFactory) ProjectsClient() (authz.ProjectsServiceClient, error) {
	conn, err := c.connectionByName("authz-service")
	if err != nil {
		return nil, err
	}
	return authz.NewProjectsServiceClient(conn), nil
}

// TeamsClient returns a client for the Teams Mgmt service.
// It requires the `teams` endpoint to be configured
func (c *clientsFactory) TeamsClient() (teams.TeamsServiceClient, error) {
	conn, err := c.connectionByName("teams-service")
	if err != nil {
		return nil, err
	}
	return teams.NewTeamsServiceClient(conn), nil
}

// TokensMgmtClient returns a client for the Tokens Mgmt service.
// It requires the `auth` endpoint to be configured
func (c *clientsFactory) TokensMgmtClient() (authn.TokensMgmtServiceClient, error) {
	conn, err := c.connectionByName("authn-service")
	if err != nil {
		return nil, err
	}
	return authn.NewTokensMgmtServiceClient(conn), nil
}

// UsersMgmtClient returns a client for the Users Mgmt service.
// It requires the `local-user-service` endpoint to be configured
func (c *clientsFactory) UsersMgmtClient() (local_user.UsersMgmtServiceClient, error) {
	conn, err := c.connectionByName("local-user-service")
	if err != nil {
		return nil, err
	}
	return local_user.NewUsersMgmtServiceClient(conn), nil
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

func (c *clientsFactory) FeedClient() (event_feed.EventFeedServiceClient, error) {
	conn, err := c.connectionByName("event-feed-service")
	if err != nil {
		return nil, err
	}
	return event_feed.NewEventFeedServiceClient(conn), nil
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

func (c *clientsFactory) LicenseControlClient() (license_control.LicenseControlServiceClient, error) {
	conn, err := c.connectionByName("license-control-service")
	if err != nil {
		return nil, err
	}
	return license_control.NewLicenseControlServiceClient(conn), nil
}

func (c *clientsFactory) DeploymentServiceClient() (deployment.DeploymentClient, error) {
	conn, err := c.connectionByName("deployment-service")
	if err != nil {
		return nil, err
	}
	return deployment.NewDeploymentClient(conn), nil
}

func (c *clientsFactory) DatafeedClient() (data_feed.DatafeedServiceClient, error) {
	conn, err := c.connectionByName("data-feed-service")
	if err != nil {
		return nil, err
	}
	return data_feed.NewDatafeedServiceClient(conn), nil
}

// PurgeClient takes a service name and returns a Purge client for the service
func (c *clientsFactory) PurgeClient(service string) (data_lifecycle.PurgeClient, error) {
	conn, err := c.connectionByName(service)
	if err != nil {
		return nil, err
	}
	return data_lifecycle.NewPurgeClient(conn), nil
}

func (c *clientsFactory) InfraProxyClient() (infra_proxy.InfraProxyServiceClient, error) {
	conn, err := c.connectionByName("infra-proxy-service")
	if err != nil {
		return nil, err
	}
	return infra_proxy.NewInfraProxyServiceClient(conn), nil
}

func (c *clientsFactory) InfraProxyMigrationClient() (infra_proxy_migration.MigrationDataServiceClient, error) {
	conn, err := c.connectionByName("infra-proxy-service")
	if err != nil {
		return nil, err
	}
	return infra_proxy_migration.NewMigrationDataServiceClient(conn), nil
}

func (c *clientsFactory) UserSettingsClient() (user_settings.UserSettingsServiceClient, error) {
	conn, err := c.connectionByName("user-settings-service")
	if err != nil {
		return nil, err
	}
	return user_settings.NewUserSettingsServiceClient(conn), nil
}

func (c *clientsFactory) connectionByName(name string) (*grpc.ClientConn, error) {
	conn, exists := c.connections[name]
	if !exists {
		err := fmt.Errorf("%w: Expected connection for %s, but none was found. Check to make sure it is correctly configured",
			ErrNoConnectionConfigured, name)
		return nil, err
	}
	return conn, nil
}

func (c *clientsFactory) Close() error {
	var err error

	for _, con := range c.connections {
		err = con.Close()
		if err != nil {
			return err
		}
	}

	return nil
}

// DialEndpoints dials the configured endpoints. If the configuration is missing
// a known gRPC service target and the null backend socket has been configured,
// it will attempt to dial the null backend.
func (c *ClientConfig) DialEndpoints(connFactory *secureconn.Factory) (ClientConnections, error) {
	connections := ClientConnections{}
	c.expandEndpoints()

	for service, endpoint := range c.Endpoints {
		metricsEnabled := clientMetrics[service]
		logctx := log.WithFields(log.Fields{
			"service": service,
			"target":  endpoint.Target,
			"secure":  endpoint.Secure,
			"metrics": metricsEnabled,
		})
		logctx.Info("Dialing")

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
			conn, err = grpc.Dial(
				endpoint.Target,
				append(opts, grpc.WithNoProxy(), grpc.WithInsecure(), tracing.GlobalClientInterceptor())...,
			)
		}

		if err != nil {
			return connections, err
		}

		connections[service] = conn
	}

	return connections, nil
}

func (c *ClientConfig) expandEndpoints() {
	if c.Endpoints == nil {
		c.Endpoints = map[string]ConnectionOptions{}
	}

	// If we've been given a null backend socket and we're missing an endpoint
	// target for a service then we'll configure it to use the null backend.
	if c.NullBackendSock != "" {
		for _, service := range grpcServices {
			// If the endpoint already exists with a target don't override it
			// with a the null backend
			if endpoint, ok := c.Endpoints[service]; ok {
				if endpoint.Target != "" {
					continue
				}
			}

			c.Endpoints[service] = ConnectionOptions{
				Target: c.socketPath(),
				Secure: false,
			}
		}
	}
}

func (c *ClientConfig) socketPath() string {
	if c.NullBackendSock == "" {
		return ""
	}

	return fmt.Sprintf("unix:%s", c.NullBackendSock)
}
