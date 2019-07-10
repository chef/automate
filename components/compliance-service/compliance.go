package compliance

import (
	"context"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"strconv"
	"time"

	"github.com/golang/mock/gomock"
	"github.com/golang/protobuf/ptypes/empty"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"

	"github.com/chef/automate/api/external/secrets"
	auth "github.com/chef/automate/api/interservice/authn"
	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	dls "github.com/chef/automate/api/interservice/data_lifecycle"
	"github.com/chef/automate/api/interservice/es_sidecar"
	"github.com/chef/automate/api/interservice/event"
	aEvent "github.com/chef/automate/api/interservice/event"
	dlsserver "github.com/chef/automate/components/compliance-service/api/datalifecycle/server"
	"github.com/chef/automate/components/compliance-service/api/jobs"
	jobsserver "github.com/chef/automate/components/compliance-service/api/jobs/server"
	"github.com/chef/automate/components/compliance-service/api/profiles"
	profilesserver "github.com/chef/automate/components/compliance-service/api/profiles/server"
	"github.com/chef/automate/components/compliance-service/api/reporting"
	reportingserver "github.com/chef/automate/components/compliance-service/api/reporting/server"
	"github.com/chef/automate/components/compliance-service/api/stats"
	statsserver "github.com/chef/automate/components/compliance-service/api/stats/server"
	"github.com/chef/automate/components/compliance-service/api/status"
	statusserver "github.com/chef/automate/components/compliance-service/api/status/server"
	"github.com/chef/automate/components/compliance-service/api/version"
	versionserver "github.com/chef/automate/components/compliance-service/api/version/server"
	"github.com/chef/automate/components/compliance-service/config"
	"github.com/chef/automate/components/compliance-service/dao/pgdb"
	"github.com/chef/automate/components/compliance-service/ingest/ingest"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	ingestserver "github.com/chef/automate/components/compliance-service/ingest/server"
	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/compliance-service/inspec-agent/remote"
	"github.com/chef/automate/components/compliance-service/inspec-agent/runner"
	"github.com/chef/automate/components/compliance-service/inspec-agent/scheduler"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/chef/automate/components/compliance-service/scanner"
	"github.com/chef/automate/components/compliance-service/utils/logging"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
	notifications "github.com/chef/automate/components/notifications-client/api"
	"github.com/chef/automate/components/notifications-client/notifier"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tracing"
)

type serviceState int

const (
	serviceStateUnknown = iota
	serviceStateStarting
	serviceStateStarted
)

var SERVICE_STATE serviceState

func createESBackend(servConf *config.Compliance) relaxting.ES2Backend {
	// define the ElasticSearch backend config with legacy automate auth
	esr := relaxting.ES2Backend{
		ESUrl:             servConf.ElasticSearch.Url,
		Enterprise:        servConf.Delivery.Enterprise,
		ChefDeliveryUser:  servConf.Delivery.User,
		ChefDeliveryToken: servConf.Delivery.Token,
	}
	return esr
}

func createPGBackend(conf *config.Postgres) (*pgdb.DB, error) {
	// define the Postgres Scanner backend
	return pgdb.New(conf)
}

// hand over the array of job ids to the scheduler to be executed by the inspec-agent
func runHungJobs(ctx context.Context, scheduledJobsIds []string, schedulerServer *scheduler.Scheduler) {
	for SERVICE_STATE != serviceStateStarted {
		time.Sleep(time.Second)
	}
	schedulerServer.RunHungJobs(ctx, scheduledJobsIds)
}

// here we execute migrations, create the es and pg backends, read certs, set up the needed env vars,
// and modify config info
func initBits(ctx context.Context, conf *config.Compliance) (db *pgdb.DB, connFactory *secureconn.Factory, esr relaxting.ES2Backend, statusSrv *statusserver.Server, err error) {
	statusSrv = statusserver.New()

	statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelPG, "Initializing DB connection and schema migration...")
	// start pg backend
	db, err = createPGBackend(&conf.Postgres)
	if err != nil {
		statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelPG, err.Error())
		statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelPG, statusserver.MigrationFailedMsg)
		return db, connFactory, esr, statusSrv, errors.Wrap(err, "createPGBackend failed")
	}
	statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelPG, statusserver.MigrationCompletedMsg)

	// create esconfig info backend
	esr = createESBackend(conf)

	backendCacheBool, err := strconv.ParseBool(conf.InspecAgent.BackendCache)
	if err != nil {
		logrus.Errorf("Unable to parse value for inspec agent backend cache configuration, %s - Valid configuration options are 'true' and 'false' ", conf.InspecAgent.BackendCache)
		inspec.BackendCache = true
	} else {
		inspec.BackendCache = backendCacheBool
	}
	inspec.TmpDir = conf.InspecAgent.TmpDir
	// Let's have something sensible if the temp dir is not specified
	if inspec.TmpDir == "" {
		inspec.TmpDir = "/tmp"
	}
	err = os.Setenv("TMPDIR", inspec.TmpDir)
	if err != nil {
		return db, connFactory, esr, statusSrv, errors.Wrap(err, "Unable to set TMPDIR env variable")
	}

	serviceCerts, err := conf.Service.ReadCerts()
	if err != nil {
		return db, connFactory, esr, statusSrv, errors.Wrap(err, "Unable to load service certificates")
	}

	connFactory = secureconn.NewFactory(*serviceCerts)
	conf.Secrets.Endpoint = fmt.Sprintf("%s:%d", conf.Secrets.HostBind, conf.Secrets.Port)
	conf.Authz.Endpoint = fmt.Sprintf("%s:%d", conf.Authz.HostBind, conf.Authz.Port)
	conf.Manager.Endpoint = fmt.Sprintf("%s:%d", conf.Manager.Host, conf.Manager.Port)
	conf.Service.Endpoint = fmt.Sprintf("%s:%d", conf.Service.HostBind, conf.Service.Port)
	return db, connFactory, esr, statusSrv, nil
}

// register all the services, start the grpc server, and call setup
func serveGrpc(ctx context.Context, db *pgdb.DB, connFactory *secureconn.Factory,
	esr relaxting.ES2Backend, conf config.Compliance, binding string,
	statusSrv *statusserver.Server) {

	lis, err := net.Listen("tcp", binding)
	if err != nil {
		logrus.Fatalf("failed to listen: %v", err)
	}

	esClient, err := esr.ES2Client()
	if err != nil {
		logrus.Fatalf("could not connect to elasticsearch: %v", err)
	}
	configManager, err := config.NewConfigManager(conf.Service.ConfigFilePath)
	if err != nil {
		logrus.Fatalf("could not create config manager: %v", err)
	}

	eventClient := getEventConnection(connFactory, conf.EventConfig.Endpoint)
	notifier := getNotificationsConnection(connFactory, conf.Notifications.Target)
	authzProjectsClient := createAuthzProjectsClient(connFactory, conf.Authz.Endpoint)
	nodeManagerServiceClient := getManagerConnection(connFactory, conf.Manager.Endpoint)
	ingesticESClient := ingestic.NewESClient(esClient)
	ingesticESClient.InitializeStore(context.Background())

	s := connFactory.NewServer(tracing.GlobalServerInterceptor())

	// needs to be the first one, since it creates the es indices
	ingest.RegisterComplianceIngesterServer(s,
		ingestserver.NewComplianceIngestServer(ingesticESClient, nodeManagerServiceClient,
			conf.InspecAgent.AutomateFQDN, notifier, authzProjectsClient, eventClient, configManager))

	jobs.RegisterJobsServiceServer(s, jobsserver.New(db, connFactory, eventClient,
		conf.Service.Endpoint, conf.Secrets.Endpoint, conf.Manager.Endpoint, conf.RemoteInspecVersion))
	reporting.RegisterReportingServiceServer(s, reportingserver.New(&esr))

	ps := profilesserver.New(db, &esr, &conf.Profiles, eventClient, statusSrv)
	profiles.RegisterProfilesServiceServer(s, ps)
	profiles.RegisterProfilesAdminServiceServer(s, ps)

	stats.RegisterStatsServiceServer(s, statsserver.New(&esr))
	version.RegisterVersionServiceServer(s, versionserver.New())
	status.RegisterComplianceStatusServer(s, statusSrv)

	dlsManageable, err := setupDataLifecycleManageableInterface(ctx, connFactory, conf)
	if err != nil {
		logrus.Fatalf("serveGrpc aborting, can't setup dlsManageable: %s", err)
	}
	dls.RegisterDataLifecycleManageableServer(s, dlsManageable)

	// Register reflection service on gRPC server.
	reflection.Register(s)
	logrus.Info("Starting GRPC server on " + binding)

	// running ElasticSearch migration
	err = relaxting.RunMigrations(esr, statusSrv)
	if err != nil {
		logrus.Fatalf("serveGrpc aborting, unable to run migrations: %v", err)
	}

	errc := make(chan error)
	defer close(errc)
	go func() {
		err := s.Serve(lis)
		errc <- errors.Wrap(err, "Serve")
	}()

	// `setup` depends on `Serve` because it dials back to the compliance-service itself.
	// For this to work we launch `Serve` in a goroutine and connect WithBlock to itself and other dependent services from `setup`
	// A connect timeout is used to ensure error reporting in the event of failures to connect
	err = setup(ctx, connFactory, conf, esr, db)
	if err != nil {
		logrus.Fatalf("serveGrpc aborting, we have a problem, setup failed: %s", err.Error())
	}

	// block here: there's at most one error we care about
	err = <-errc

	// if we reach this, we've had an issue in Serve()
	logrus.Fatalf("serveGrpc aborting, we have a problem: %s", err.Error())
}

func getEventConnection(connectionFactory *secureconn.Factory,
	eventEndpoint string) aEvent.EventServiceClient {
	if eventEndpoint == "" || eventEndpoint == ":0" {
		if os.Getenv("RUN_MODE") == "test" {
			logrus.Infof("using mock Event service Client")
			eventServiceClientMock := event.NewMockEventServiceClient(gomock.NewController(nil))
			eventServiceClientMock.EXPECT().Publish(gomock.Any(), gomock.Any()).AnyTimes().Return(
				&event.PublishResponse{}, nil)
			return eventServiceClientMock
		}
		logrus.Fatalf("eventEndpoint cannot be empty or Dial will get stuck")
	}

	logrus.Debugf("Connecting to event-service %q", eventEndpoint)
	timeoutCtx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()
	conn, err := connectionFactory.DialContext(timeoutCtx, "event-service",
		eventEndpoint, grpc.WithBlock())
	if err != nil {
		logrus.Fatalf("compliance setup, error grpc dialing to event-service aborting...")
	}
	// get event client
	eventClient := aEvent.NewEventServiceClient(conn)
	if eventClient == nil {
		logrus.Fatalf("compliance setup, could not obtain automate events service client: %s", err)
	}

	return eventClient
}

func getNotificationsConnection(connectionFactory *secureconn.Factory,
	notificationsEndpoint string) notifier.Notifier {
	if notificationsEndpoint == "" || notificationsEndpoint == ":0" {
		if os.Getenv("RUN_MODE") == "test" {
			logrus.Infof("using mock Notifications")
			return &NotifierMock{}
		}
		logrus.Fatalf("notificationsEndpoint cannot be empty or Dial will get stuck")
	}

	logrus.Debugf("Connecting to notifications-service %q", notificationsEndpoint)
	timeoutCtx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()
	conn, err := connectionFactory.DialContext(timeoutCtx, "notifications-service",
		notificationsEndpoint, grpc.WithBlock())
	if err != nil {
		logrus.Fatalf("getNotificationsConnection, error grpc dialing to manager %s", err.Error())
	}

	notifier := notifier.New(conn)
	if notifier == nil {
		logrus.Fatalf("compliance setup, could not obtain notification client: %s", err)
	}

	return notifier
}

func createAuthzProjectsClient(connectionFactory *secureconn.Factory,
	authzEndpoint string) iam_v2.ProjectsClient {
	if authzEndpoint == "" || authzEndpoint == ":0" {
		if os.Getenv("RUN_MODE") == "test" {
			logrus.Infof("using mock ProjectsClient")
			// If any other rpc function other than the ListRulesForAllProjects is used the process will fail.
			mockProjectsClient := iam_v2.NewMockProjectsClient(gomock.NewController(nil))
			mockProjectsClient.EXPECT().ListRulesForAllProjects(gomock.Any(), gomock.Any()).Return(
				&iam_v2.ListRulesForAllProjectsResp{}, nil)
			return mockProjectsClient
		}
		logrus.Fatal("authzEndpoint cannot be empty or Dial will get stuck")
	}

	logrus.Debugf("Connecting to authz-service %q", authzEndpoint)
	timeoutCtx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()
	conn, err := connectionFactory.DialContext(timeoutCtx, "authz-service",
		authzEndpoint, grpc.WithBlock())
	if err != nil {
		logrus.Fatalf("getAuthzConnection, error grpc dialing to Authz %s", err.Error())
	}

	authzProjectsClient := iam_v2.NewProjectsClient(conn)
	if authzProjectsClient == nil {
		logrus.Fatalf("getAuthzConnection got nil for NewProjectsClient")
	}

	return authzProjectsClient
}

func getManagerConnection(connectionFactory *secureconn.Factory,
	managerEndpoint string) manager.NodeManagerServiceClient {
	if managerEndpoint == "" || managerEndpoint == ":0" {
		if os.Getenv("RUN_MODE") == "test" {
			logrus.Infof("using mock NodeManagerMock")
			return &NodeManagerMock{}
		}
		logrus.Fatal("managerEndpoint cannot be empty or Dial will get stuck")
	}

	logrus.Debugf("Connecting to nodemanager-service %q", managerEndpoint)
	timeoutCtx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()
	conn, err := connectionFactory.DialContext(timeoutCtx, "nodemanager-service",
		managerEndpoint, grpc.WithBlock())
	if err != nil {
		logrus.Fatalf("getManagerConnection, error grpc dialing to manager %s", err.Error())
	}

	mgrClient := manager.NewNodeManagerServiceClient(conn)
	if mgrClient == nil {
		logrus.Fatalf("getManagerConnection got nil for NewNodeManagerServiceClient")
	}

	return mgrClient
}

func setupDataLifecycleManageableInterface(ctx context.Context, connFactory *secureconn.Factory,
	conf config.Compliance) (*dlsserver.DataLifecycleManageableServer, error) {
	purgePolicies := []dlsserver.PurgePolicy{}
	if conf.ComplianceReportDays > 0 {
		purgePolicies = append(purgePolicies, dlsserver.PurgePolicy{
			IndexName:          fmt.Sprintf("comp-%s-s", mappings.ComplianceCurrentTimeSeriesIndicesVersion),
			PurgeOlderThanDays: conf.ComplianceReportDays,
		})
		purgePolicies = append(purgePolicies, dlsserver.PurgePolicy{
			IndexName:          fmt.Sprintf("comp-%s-r", mappings.ComplianceCurrentTimeSeriesIndicesVersion),
			PurgeOlderThanDays: conf.ComplianceReportDays,
		})
	}

	var err error
	var esSidecarConn *grpc.ClientConn
	if os.Getenv("RUN_MODE") == "test" {
		logrus.Infof(`Skipping ES sidecar-service dial due to RUN_MODE env var being set to "test"`)
	} else {
		timeoutCtx, cancel := context.WithTimeout(ctx, 60*time.Second)
		defer cancel()
		// Data Lifecycle Interface
		logrus.Infof("Connecting to %s", conf.ESSidecarAddress)
		esSidecarConn, err = connFactory.DialContext(timeoutCtx, "es-sidecar-service",
			conf.ESSidecarAddress, grpc.WithBlock())
		if err != nil || esSidecarConn == nil {
			logrus.WithFields(logrus.Fields{"error": err}).Fatal("Failed to create ES Sidecar connection")
			return nil, err
		}
	}
	return dlsserver.NewDataLifecycleManageableServer(es_sidecar.NewEsSidecarClient(esSidecarConn), purgePolicies), nil
}

func setup(ctx context.Context, connFactory *secureconn.Factory, conf config.Compliance,
	esr relaxting.ES2Backend, db *pgdb.DB) error {
	var err error
	var conn, mgrConn, secretsConn, authConn *grpc.ClientConn
	timeoutCtx, cancel := context.WithTimeout(ctx, 15*time.Second)
	defer cancel()

	// get compliance connection for ingest
	logrus.Debugf("compliance setup, dialing compliance-service manager(%s)", conf.Service.Endpoint)
	conn, err = connFactory.DialContext(timeoutCtx, "compliance-service", conf.Service.Endpoint, grpc.WithBlock())
	if err != nil || conn == nil {
		err = errors.New("compliance setup, error grpc dialing to compliance-service...")
		return err
	}

	// get ingest client
	logrus.Debugf("compliance setup, getting an ingest client")
	ingestClient := ingest.NewComplianceIngesterClient(conn)
	if ingestClient == nil {
		return fmt.Errorf("compliance setup, got nil for NewComplianceIngesterClient")
	}

	// get nodemanager connection
	logrus.Debugf("compliance setup, dialing nodemanager(%s)", conf.Manager.Endpoint)
	mgrConn, err = connFactory.DialContext(timeoutCtx, "nodemanager-service", conf.Manager.Endpoint, grpc.WithBlock())
	if err != nil || mgrConn == nil {
		err = errors.New("compliance setup, error grpc dialing to manager aborting...")
		return err
	}

	// get nodemanager client
	logrus.Debugf("compliance setup, getting a node manager client")
	mgrClient := manager.NewNodeManagerServiceClient(mgrConn)
	if mgrClient == nil {
		return fmt.Errorf("compliance setup, got nil for NewNodeManagerServiceClient")
	}

	// get nodes client
	logrus.Debugf("compliance setup, getting a nodes client")
	nodesClient := nodes.NewNodesServiceClient(mgrConn)
	if nodesClient == nil {
		return fmt.Errorf("compliance setup, got nil for NewNodesServiceClient")
	}

	// get secrets connection
	logrus.Debugf("compliance setup, dialing secrets-service(%s)", conf.Secrets.Endpoint)
	secretsConn, err = connFactory.DialContext(timeoutCtx, "secrets-service", conf.Secrets.Endpoint, grpc.WithBlock())
	if err != nil {
		return fmt.Errorf("compliance setup, error grpc dialing to secrets")
	}

	// get secrets client
	logrus.Debugf("compliance setup, getting a secrets service client")
	secretsClient := secrets.NewSecretsServiceClient(secretsConn)
	if secretsClient == nil {
		return fmt.Errorf("compliance setup, could not obtain secrets service client")
	}

	// set up the scanner, scheduler, and runner servers with needed clients
	// these are all inspec-agent packages
	scannerServer := scanner.New(mgrClient, nodesClient, db)
	schedulerServer := scheduler.New(mgrClient, nodesClient, db, ingestClient, secretsClient, conf.RemoteInspecVersion)
	runnerServer := runner.New(mgrClient, nodesClient, db, ingestClient, conf.RemoteInspecVersion)

	// start polling for jobs with a recurrence schedule that are due to run.
	// this function will sleep for one minute, then query the db for all jobs
	// with recurrence and check if it's time to run the job
	go schedulerServer.PollForJobs(ctx)

	// start the InspecAgent workers.  the workers represent the amount of goroutines
	// available to execute a scan.  the buffer size represents the maximum amount of jobs
	// that may get backed up in the agent queue
	logrus.Infof("compliance service initializing %d job workers with %d job buffer size",
		conf.InspecAgent.JobWorkers, conf.InspecAgent.JobBufferSize)
	runnerServer.SetWorkers(conf.InspecAgent.JobWorkers, conf.InspecAgent.JobBufferSize)

	if os.Getenv("RUN_MODE") == "test" {
		logrus.Infof(`Skipping AUTHN client setup due to RUN_MODE env var being set to "test"`)
	} else {
		// get the authn-service connection
		logrus.Debugf("compliance setup, dialing authn-service(%s)", conf.InspecAgent.AuthnTarget)
		authConn, err = connFactory.DialContext(timeoutCtx, "authn-service", conf.InspecAgent.AuthnTarget,
			grpc.WithBlock())
		if err != nil || authConn == nil {
			err = errors.New("compliance setup, error grpc dialing to authn aborting...")
			return err
		}
		// get the authn client
		authClient := auth.NewTokensMgmtClient(authConn)
		if authClient == nil {
			logrus.Errorf("serveGrpc got nil for NewTokensMgmtClient: %s", err)
			return err
		}
		// in order to execute scan jobs remotely (i.e. on a different server, reporting back out
		// to automate), we need access to the auth client for a token and the automate fqdn for reporting
		remote.RemoteJobInfo = remote.RemoteJob{
			TokensMgmtClient: authClient,
			AutomateFQDN:     conf.InspecAgent.AutomateFQDN,
		}
	}

	SERVICE_STATE = serviceStateStarted
	go checkAndRunHungJobs(ctx, scannerServer, schedulerServer)
	return nil
}

func checkAndRunHungJobs(ctx context.Context, scannerServer *scanner.Scanner,
	schedulerServer *scheduler.Scheduler) {
	// check for 'abandoned' jobs by querying for jobs with a status of running or scheduled
	// of type exec that have not been marked for deletion. jobs with a status of running will
	// be marked as aborted.  only jobs with a status of scheduled will be returned.
	// this is done b/c the inspec-agent is ephemeral -- if a service restart occurs when the agent
	// has already been given jobs, it will lose all references to those jobs
	scheduledJobsIds, err := scannerServer.CheckForHungJobs(ctx)
	if err != nil {
		logrus.Errorf("unable to check for abandoned jobs %+v", err)
	} else {
		// hand over all 'abandoned' jobs with status scheduled to the scheduler
		runHungJobs(ctx, scheduledJobsIds, schedulerServer)
	}
}

// Serve grpc
func Serve(conf config.Compliance, grpcBinding string) error {
	SERVICE_STATE = serviceStateUnknown
	logging.SetLogLevel(conf.Service.LogLevel)

	ctx := context.Background()
	db, connFactory, esr, statusSrv, err := initBits(ctx, &conf)
	if err != nil {
		return err
	}
	SERVICE_STATE = serviceStateStarting
	go serveGrpc(ctx, db, connFactory, esr, conf, grpcBinding, statusSrv) // nolint: errcheck

	cfg := NewServiceConfig(&conf, connFactory)
	return cfg.serveCustomRoutes()
}

// ServiceInfo holds service listen info
type ServiceInfo struct {
	HostBind   string
	Port       int
	ServerBind string

	connFactory *secureconn.Factory
}

//TODO(jaym) If these don't get exposed in the gateway, we need to provide the http server certs
// this custom route is used by the inspec-agent scanner to retrieve profile tars for scan execution
func (conf *ServiceInfo) serveCustomRoutes() error {
	conf.ServerBind = fmt.Sprintf("%s:%d", conf.HostBind, conf.Port)
	serveAddress := fmt.Sprintf("127.0.0.1:%d", 2133) // Similarly hard-coded in inspec-agent

	_, cancel := context.WithCancel(context.Background())
	defer cancel()

	r := http.NewServeMux()

	r.HandleFunc("/profiles/tar", conf.ProfileTarHandler)

	return http.ListenAndServe(serveAddress, r)
}

// NewServiceConfig returns a ServiceInfo instance
func NewServiceConfig(cfg *config.Compliance, connFactory *secureconn.Factory) *ServiceInfo {
	return &ServiceInfo{
		HostBind: cfg.Service.HostBind,
		Port:     cfg.Service.Port,

		connFactory: connFactory,
	}
}

// ProfileTarHandler is the http handler for profile tarballs, used by the inspec-agent
// for executing scans
func (conf *ServiceInfo) ProfileTarHandler(w http.ResponseWriter, r *http.Request) {
	if err := r.ParseForm(); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	profileName := r.Form.Get("name")
	profileVersion := r.Form.Get("version")
	profileOwner := r.Form.Get("owner")

	conn, err := conf.connFactory.Dial("compliance-service", conf.ServerBind)
	if err != nil {
		msg := fmt.Sprintf("grpc service unavailable %s", conf.ServerBind)
		http.Error(w, msg, http.StatusServiceUnavailable)
		return
	}
	defer conn.Close() // nolint: errcheck

	profilesClient := profiles.NewProfilesServiceClient(conn)

	stream, err := profilesClient.ReadTar(context.Background(),
		&profiles.ProfileDetails{Name: profileName, Version: profileVersion, Owner: profileOwner})
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	for {
		data, err := stream.Recv()
		if err == io.EOF {
			break
		}
		if err != nil {
			http.Error(w, err.Error(), http.StatusNotFound)
			return
		}
		contentLength := strconv.Itoa(len(data.GetData()))
		w.Header().Set("Content-Length", contentLength)
		w.Header().Set("Content-Type", "application/x-gzip")
		w.Header().Set("Accept-Ranges", "bytes")
		w.Write(data.GetData()) // nolint: errcheck
	}
}

type NotifierMock struct {
}

func (n *NotifierMock) Send(context.Context, *notifications.Event) {

}

func (n *NotifierMock) QueueSize() int {
	return 0
}

type NodeManagerMock struct {
}

func (nm *NodeManagerMock) Create(ctx context.Context, in *manager.NodeManager,
	opts ...grpc.CallOption) (*manager.Ids, error) {
	return &manager.Ids{}, nil
}

func (nm *NodeManagerMock) Read(ctx context.Context, in *manager.Id,
	opts ...grpc.CallOption) (*manager.NodeManager, error) {
	return &manager.NodeManager{}, nil
}
func (nm *NodeManagerMock) Update(ctx context.Context, in *manager.NodeManager,
	opts ...grpc.CallOption) (*empty.Empty, error) {
	return &empty.Empty{}, nil
}

func (nm *NodeManagerMock) Delete(ctx context.Context, in *manager.Id,
	opts ...grpc.CallOption) (*empty.Empty, error) {
	return &empty.Empty{}, nil
}

func (nm *NodeManagerMock) DeleteWithNodes(ctx context.Context, in *manager.Id,
	opts ...grpc.CallOption) (*manager.Ids, error) {
	return &manager.Ids{}, nil
}

func (nm *NodeManagerMock) DeleteWithNodeStateStopped(ctx context.Context, in *manager.Id,
	opts ...grpc.CallOption) (*empty.Empty, error) {
	return &empty.Empty{}, nil
}

func (nm *NodeManagerMock) DeleteWithNodeStateTerminated(ctx context.Context, in *manager.Id,
	opts ...grpc.CallOption) (*empty.Empty, error) {
	return &empty.Empty{}, nil
}

func (nm *NodeManagerMock) List(ctx context.Context, in *manager.Query,
	opts ...grpc.CallOption) (*manager.NodeManagers, error) {
	return &manager.NodeManagers{}, nil
}

func (nm *NodeManagerMock) Connect(ctx context.Context, in *manager.NodeManager,
	opts ...grpc.CallOption) (*empty.Empty, error) {
	return &empty.Empty{}, nil
}

func (nm *NodeManagerMock) ConnectManager(ctx context.Context, in *manager.Id,
	opts ...grpc.CallOption) (*empty.Empty, error) {
	return &empty.Empty{}, nil
}

func (nm *NodeManagerMock) SearchNodeFields(ctx context.Context, in *manager.FieldQuery,
	opts ...grpc.CallOption) (*manager.Fields, error) {
	return &manager.Fields{}, nil
}

func (nm *NodeManagerMock) SearchNodes(ctx context.Context, in *manager.NodeQuery,
	opts ...grpc.CallOption) (*manager.Nodes, error) {
	return &manager.Nodes{}, nil
}

func (nm *NodeManagerMock) ProcessNode(ctx context.Context, in *manager.NodeMetadata,
	opts ...grpc.CallOption) (*manager.ProcessNodeResponse, error) {
	return &manager.ProcessNodeResponse{}, nil
}

func (nm *NodeManagerMock) ChangeNodeState(ctx context.Context, in *manager.NodeState,
	opts ...grpc.CallOption) (*manager.ChangeNodeStateResponse, error) {
	return &manager.ChangeNodeStateResponse{}, nil
}

func (nm *NodeManagerMock) GetNodeWithSecrets(ctx context.Context, in *manager.Id,
	opts ...grpc.CallOption) (*nodes.Node, error) {
	return &nodes.Node{}, nil
}

func (nm *NodeManagerMock) SearchManagerNodes(ctx context.Context, in *manager.NodeQuery,
	opts ...grpc.CallOption) (*manager.ManagerNodes, error) {
	return &manager.ManagerNodes{}, nil
}
