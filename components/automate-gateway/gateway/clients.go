package gateway

import (
	"fmt"
	"time"

	grpc_prometheus "github.com/grpc-ecosystem/go-grpc-prometheus"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/api/external/data_feed"
	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/api/interservice/authn"
	"github.com/chef/automate/api/interservice/authz"
	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	cfgmgmt "github.com/chef/automate/api/interservice/cfgmgmt/service"
	"github.com/chef/automate/api/interservice/data_lifecycle"
	"github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/api/interservice/event_feed"
	infra_proxy "github.com/chef/automate/api/interservice/infra_proxy/service"
	chef_ingest "github.com/chef/automate/api/interservice/ingest"
	"github.com/chef/automate/api/interservice/license_control"
	"github.com/chef/automate/api/interservice/local_user"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	teams "github.com/chef/automate/api/interservice/teams/v2"
	"github.com/chef/automate/components/compliance-service/api/jobs"
	profiles "github.com/chef/automate/components/compliance-service/api/profiles"
	cc_reporting "github.com/chef/automate/components/compliance-service/api/reporting"
	cc_stats "github.com/chef/automate/components/compliance-service/api/stats"
	cc_version "github.com/chef/automate/components/compliance-service/api/version"
	cc_ingest "github.com/chef/automate/components/compliance-service/ingest/ingest"
	notifications "github.com/chef/automate/components/notifications-client/api"
	"github.com/chef/automate/components/notifications-client/notifier"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tracing"
)

// grpcServices are gRPC backend services that we'll connect to to build clients
var grpcServices = []string{
	"applications-service",
	"authn-service",
	"authz-service",
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
	CfgMgmtClient() (cfgmgmt.CfgMgmtClient, error)
	IngestStatusClient() (chef_ingest.IngestStatusClient, error)
	ChefIngesterClient() (chef_ingest.ChefIngesterClient, error)
	ChefIngesterJobSchedulerClient() (chef_ingest.JobSchedulerClient, error)
	ComplianceIngesterClient() (cc_ingest.ComplianceIngesterClient, error)
	NotificationsClient() (notifications.NotificationsClient, error)
	AuthenticationClient() (authn.AuthenticationClient, error)
	AuthorizationClient() (authz.AuthorizationClient, error)
	AuthorizationV2Client() (iam_v2.AuthorizationClient, error)
	PoliciesClient() (iam_v2.PoliciesClient, error)
	ProjectsClient() (iam_v2.ProjectsClient, error)
	TeamsClient() (teams.TeamsV2Client, error)
	TokensMgmtClient() (authn.TokensMgmtClient, error)
	UsersMgmtClient() (local_user.UsersMgmtClient, error)
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
	LicenseControlClient() (license_control.LicenseControlClient, error)
	DeploymentServiceClient() (deployment.DeploymentClient, error)
	DatafeedClient() (data_feed.DatafeedServiceClient, error)
	PurgeClient(service string) (data_lifecycle.PurgeClient, error)
	InfraProxyClient() (infra_proxy.InfraProxyClient, error)
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
func (c *clientsFactory) AuthorizationV2Client() (iam_v2.AuthorizationClient, error) {
	conn, err := c.connectionByName("authz-service")
	if err != nil {
		return nil, err
	}
	return iam_v2.NewAuthorizationClient(conn), nil
}

// PoliciesClient returns a client for the Policies service.
// It requires the `authz` endpoint to be configured
func (c *clientsFactory) PoliciesClient() (iam_v2.PoliciesClient, error) {
	conn, err := c.connectionByName("authz-service")
	if err != nil {
		return nil, err
	}
	return iam_v2.NewPoliciesClient(conn), nil
}

// ProjectsClient returns a client for the Projects service.
// It requires the `authz` endpoint to be configured
func (c *clientsFactory) ProjectsClient() (iam_v2.ProjectsClient, error) {
	conn, err := c.connectionByName("authz-service")
	if err != nil {
		return nil, err
	}
	return iam_v2.NewProjectsClient(conn), nil
}

// TeamsClient returns a client for the Teams Mgmt service.
// It requires the `teams` endpoint to be configured
func (c *clientsFactory) TeamsClient() (teams.TeamsV2Client, error) {
	conn, err := c.connectionByName("teams-service")
	if err != nil {
		return nil, err
	}
	return teams.NewTeamsV2Client(conn), nil
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

func (c *clientsFactory) InfraProxyClient() (infra_proxy.InfraProxyClient, error) {
	conn, err := c.connectionByName("infra-proxy-service")
	if err != nil {
		return nil, err
	}
	return infra_proxy.NewInfraProxyClient(conn), nil
}

func (c *clientsFactory) connectionByName(name string) (*grpc.ClientConn, error) {
	conn, exists := c.connections[name]
	if !exists {
		err := fmt.Errorf("Expected connection for %s, but none was found. Check to make sure it is correctly configured", name)
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

		if endpoint.Target == c.socketPath() {
			opts = append(opts, grpc.WithBlock(), grpc.WithTimeout(1*time.Second))
		}

		if endpoint.Secure {
			conn, err = connFactory.Dial(service, endpoint.Target, opts...)
		} else {
			conn, err = grpc.Dial(
				endpoint.Target,
				append(opts, grpc.WithInsecure(), tracing.GlobalClientInterceptor())...,
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

	// If we've been given a null backend socket and we're missing and enpoint
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
