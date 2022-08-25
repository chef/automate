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

	"github.com/chef/automate/components/compliance-service/ingest/pipeline/processor"
	"github.com/chef/automate/components/compliance-service/inspec-agent/resolver"
	"github.com/chef/automate/components/compliance-service/migrations"

	"github.com/pkg/errors"

	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/api/interservice/authn"
	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/api/interservice/compliance/ingest/ingest"
	"github.com/chef/automate/api/interservice/compliance/jobs"
	"github.com/chef/automate/api/interservice/compliance/profiles"
	"github.com/chef/automate/api/interservice/compliance/reporting"
	"github.com/chef/automate/api/interservice/compliance/stats"
	"github.com/chef/automate/api/interservice/compliance/status"
	"github.com/chef/automate/api/interservice/compliance/version"
	"github.com/chef/automate/api/interservice/data_lifecycle"
	"github.com/chef/automate/api/interservice/es_sidecar"
	"github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	reportmanager "github.com/chef/automate/api/interservice/report_manager"
	jobsserver "github.com/chef/automate/components/compliance-service/api/jobs/server"
	profilesserver "github.com/chef/automate/components/compliance-service/api/profiles/server"
	reportingserver "github.com/chef/automate/components/compliance-service/api/reporting/server"
	statsserver "github.com/chef/automate/components/compliance-service/api/stats/server"
	statusserver "github.com/chef/automate/components/compliance-service/api/status/server"
	versionserver "github.com/chef/automate/components/compliance-service/api/version/server"
	"github.com/chef/automate/components/compliance-service/config"
	"github.com/chef/automate/components/compliance-service/dao/pgdb"
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
	notifications "github.com/chef/automate/components/notifications-client/api"
	"github.com/chef/automate/components/notifications-client/notifier"
	project_update_lib "github.com/chef/automate/lib/authz"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/postgres"
	"github.com/chef/automate/lib/datalifecycle/purge"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tracing"
	"github.com/golang/mock/gomock"
	"github.com/golang/protobuf/ptypes/empty"
	"github.com/sirupsen/logrus"
	"github.com/teambition/rrule-go"
	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"
)

type serviceState int

const (
	serviceStateUnknown = iota
	serviceStateStarting
	serviceStateStarted
)

var SERVICE_STATE serviceState

var (
	PurgeWorkflowName = cereal.NewWorkflowName("purge")
	PurgeScheduleName = "periodic_purge"
)

func createESBackend(servConf *config.Compliance, db *pgdb.DB) relaxting.ES2Backend {

	// define the ElasticSearch backend config with legacy automate auth
	esr := relaxting.ES2Backend{
		ESUrl:             servConf.ElasticSearch.Url,
		Enterprise:        servConf.Delivery.Enterprise,
		ChefDeliveryUser:  servConf.Delivery.User,
		ChefDeliveryToken: servConf.Delivery.Token,
		PGdb:              db,
	}
	return esr
}

func createPGBackend(conf *config.Postgres) (*pgdb.DB, error) {
	// define the Postgres Scanner backend
	return pgdb.New(conf)
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
	esr = createESBackend(conf, db)

	backendCacheBool, err := strconv.ParseBool(conf.InspecAgent.BackendCache)
	if err != nil {
		logrus.Errorf("Unable to parse value for inspec agent backend cache configuration, %s - Valid configuration options are 'true' and 'false' ", conf.InspecAgent.BackendCache)
		inspec.BackendCache = true
	} else {
		inspec.BackendCache = backendCacheBool
	}

	inspec.ResultMessageLimit = conf.InspecAgent.ResultMessageLimit
	runner.ControlResultsLimit = conf.InspecAgent.ControlResultsLimit
	runner.RunTimeLimit = conf.InspecAgent.RunTimeLimit

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
	statusSrv *statusserver.Server, cerealManager *cereal.Manager) {

	lis, err := net.Listen("tcp", binding)
	if err != nil {
		logrus.Fatalf("failed to listen: %v", err)
	}

	esClient, err := esr.ES2Client()
	if err != nil {
		logrus.Fatalf("could not connect to elasticsearch: %v", err)
	}

	var authzProjectsClient authz.ProjectsServiceClient
	eventClient := getEventConnection(connFactory, conf.EventConfig.Endpoint)
	notifier := getNotificationsConnection(connFactory, conf.Notifications.Target)
	if os.Getenv("RUN_MODE") != "test" {
		authzProjectsClient = createAuthzProjectsClient(connFactory, conf.Authz.Endpoint)
	} else {
		logrus.Infof("not getting authz client; env var RUN_MODE found. value is 'test' ")
	}
	nodeManagerServiceClient := getManagerConnection(connFactory, conf.Manager.Endpoint)
	ingesticESClient := ingestic.NewESClient(esClient)
	ingesticESClient.InitializeStore(context.Background())
	runner.ESClient = ingesticESClient
	var reportmanagerClient reportmanager.ReportManagerServiceClient
	reportmanagerClient = nil
	if conf.Service.EnableLargeReporting {
		reportmanagerClient = createReportManager(connFactory, conf.ReportConfig.Endpoint)
	}

	s := connFactory.NewServer(tracing.GlobalServerInterceptor())

	if os.Getenv("RUN_MODE") == "test" {
		logrus.Warn(`Skipping project-update manager setup due to RUN_MODE env var being set to "test"`)
	} else {
		cerealProjectUpdateManager, err := createProjectUpdateCerealManager(connFactory, conf.CerealConfig.Endpoint)
		if err != nil {
			logrus.WithError(err).Fatal("could not create cereal manager")
		}
		err = project_update_lib.RegisterTaskExecutors(cerealProjectUpdateManager, "compliance", ingesticESClient, authzProjectsClient)
		if err != nil {
			logrus.WithError(err).Fatal("could not register project update task executors")
		}
		err = project_update_lib.RegisterSerialTaskExecutors(cerealProjectUpdateManager, "compliance", ingesticESClient, authzProjectsClient)
		if err != nil {
			logrus.WithError(err).Fatal("could not register project update task executors")
		}
		if err := cerealProjectUpdateManager.Start(ctx); err != nil {
			logrus.WithError(err).Fatal("could not start cereal manager")
		}
	}

	upgradeDB := pgdb.NewDB(db)
	upgradeService := migrations.NewService(upgradeDB, cerealManager)

	// Initiating cereal Manager for upgrade jobs
	err = migrations.InitCerealManager(cerealManager, 1, ingesticESClient, upgradeDB)
	if err != nil {
		logrus.Fatalf("Failed to initiate cereal manager for upgrading jobs %v", err)
	}

	err = processor.InitCerealManager(cerealManager, conf.CerealConfig.Workers, ingesticESClient)
	if err != nil {
		logrus.Fatalf("failed to initiate cereal manager: %v", err)
	}

	// needs to be the first one, since it creates the es indices
	ingest.RegisterComplianceIngesterServiceServer(s,
		ingestserver.NewComplianceIngestServer(ingesticESClient, nodeManagerServiceClient,
			reportmanagerClient, conf.InspecAgent.AutomateFQDN, notifier, authzProjectsClient,
			conf.Service.MessageBufferSize, conf.Service.EnableLargeReporting, cerealManager))

	jobs.RegisterJobsServiceServer(s, jobsserver.New(db, connFactory, eventClient,
		conf.Manager.Endpoint, cerealManager))
	reporting.RegisterReportingServiceServer(s, reportingserver.New(&esr, reportmanagerClient,
		conf.Service.LcrOpenSearchRequests, db))

	ps := profilesserver.New(db, &esr, ingesticESClient, &conf.Profiles, eventClient, statusSrv)
	profiles.RegisterProfilesServiceServer(s, ps)
	profiles.RegisterProfilesAdminServiceServer(s, ps)

	stats.RegisterStatsServiceServer(s, statsserver.New(&esr, db))
	version.RegisterVersionServiceServer(s, versionserver.New())
	status.RegisterComplianceStatusServiceServer(s, statusSrv)

	if os.Getenv("RUN_MODE") == "test" {
		logrus.Warn(`Skipping data-lifecycle setup due to RUN_MODE env var being set to "test"`)
	} else {
		purgeServer, err := setupDataLifecyclePurgeInterface(ctx, connFactory, conf, cerealManager)
		if err != nil {
			logrus.Fatalf("serveGrpc aborting, can't setup purge server: %s", err)
		}
		data_lifecycle.RegisterPurgeServer(s, purgeServer)
	}

	// Register reflection service on gRPC server.
	reflection.Register(s)
	logrus.Info("Starting GRPC server on " + binding)

	// check the index setting
	maxInnerResults, err := relaxting.GetMaxInnerResultWindow(esr)
	if err != nil {
		logrus.Fatalf("serveGrpc aborting, unable to get max inner results window of indices: %v", err)
	}
	if maxInnerResults != int64(10000) {
		err = relaxting.SetMaxInnerResultWindow(esr)
		if err != nil {
			logrus.Fatalf("serveGrpc aborting, unable to set max inner results window of indices: %v", err)
		}
	}

	// running ElasticSearch migration
	err = relaxting.RunMigrations(esr, statusSrv)
	if err != nil {
		logrus.Fatalf("serveGrpc aborting, unable to run migrations: %v", err)
	}

	// Running upgrade scenarios for DayLatest flag
	go upgradeService.PollForUpgradeFlagDayLatest()

	errc := make(chan error)
	defer close(errc)
	go func() {
		err := s.Serve(lis)
		errc <- errors.Wrap(err, "Serve")
	}()

	// `setup` depends on `Serve` because it dials back to the compliance-service itself.
	// For this to work we launch `Serve` in a goroutine and connect WithBlock to itself and other dependent services from `setup`
	// A connect timeout is used to ensure error reporting in the event of failures to connect
	err = setup(ctx, connFactory, conf, esr, db, cerealManager)
	if err != nil {
		logrus.Fatalf("serveGrpc aborting, we have a problem, setup failed: %s", err.Error())
	}

	// block here: there's at most one error we care about
	err = <-errc

	// if we reach this, we've had an issue in Serve()
	logrus.Fatalf("serveGrpc aborting, we have a problem: %s", err.Error())
}

func createProjectUpdateCerealManager(connFactory *secureconn.Factory, address string) (*cereal.Manager, error) {
	conn, err := connFactory.Dial("cereal-service", address)
	if err != nil {
		return nil, errors.Wrap(err, "error dialing cereal service")
	}

	grpcBackend := project_update_lib.ProjectUpdateBackend(conn)
	manager, err := cereal.NewManager(grpcBackend)
	if err != nil {
		grpcBackend.Close() // nolint: errcheck
		return nil, err
	}

	return manager, nil
}

func getEventConnection(connectionFactory *secureconn.Factory,
	eventEndpoint string) event.EventServiceClient {
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
	eventClient := event.NewEventServiceClient(conn)
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
	authzEndpoint string) authz.ProjectsServiceClient {
	if authzEndpoint == "" || authzEndpoint == ":0" {
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

	authzProjectsClient := authz.NewProjectsServiceClient(conn)
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

func createReportManager(connFactory *secureconn.Factory, address string) reportmanager.ReportManagerServiceClient {
	if address == "" || address == ":0" {
		logrus.Fatal("report-manager cannot be empty or Dial will get stuck")
	}

	logrus.Debugf("Connecting to report-manager %q", address)
	timeoutCtx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()
	conn, err := connFactory.DialContext(timeoutCtx, "report-manager-service",
		address, grpc.WithBlock())
	if err != nil {
		logrus.Fatalf("report-manager, error grpc dialing to manager %s", err.Error())
	}
	mgrClient := reportmanager.NewReportManagerServiceClient(conn)

	return mgrClient
}

func setupDataLifecyclePurgeInterface(ctx context.Context, connFactory *secureconn.Factory,
	conf config.Compliance, cerealManager *cereal.Manager) (*purge.Server, error) {

	var (
		compSIndex           = fmt.Sprintf("comp-%s-s", mappings.ComplianceCurrentTimeSeriesIndicesVersion)
		compSName            = "compliance-scans"
		compRIndex           = fmt.Sprintf("comp-%s-r", mappings.ComplianceCurrentTimeSeriesIndicesVersion)
		compRName            = "compliance-reports"
		defaultPurgePolicies = &purge.Policies{
			Es: map[string]purge.EsPolicy{
				compSName: {
					Name:          compSName,
					IndexName:     compSIndex,
					OlderThanDays: conf.ComplianceReportDays,
				},
				compRName: {
					Name:          compRName,
					IndexName:     compRIndex,
					OlderThanDays: conf.ComplianceReportDays,
				},
			},
		}
		err             error
		esSidecarConn   *grpc.ClientConn
		esSidecarClient es_sidecar.EsSidecarServiceClient
		recurrence      *rrule.RRule
	)

	// Migrate default policy values from the config. The default policies are
	// only persisted the first time the workflow is created, after which only
	// new default policies are added and/or existing policies indices are
	// updated in case they have been migrated.

	for i, p := range defaultPurgePolicies.Es {
		if conf.ComplianceReportDays < 0 {
			p.Disabled = true
		}
		p.OlderThanDays = conf.ComplianceReportDays
		defaultPurgePolicies.Es[i] = p
	}

	timeoutCtx, cancel := context.WithTimeout(ctx, 60*time.Second)
	defer cancel()

	addr := conf.ElasticSearchSidecar.Address
	logrus.WithField("address", addr).Info("Connecting to Elasticsearch Sidecar")

	esSidecarConn, err = connFactory.DialContext(timeoutCtx, "es-sidecar-service",
		addr, grpc.WithBlock())
	if err != nil || esSidecarConn == nil {
		logrus.WithFields(logrus.Fields{"error": err}).Fatal("Failed to create ES Sidecar connection")
		return nil, err
	}
	esSidecarClient = es_sidecar.NewEsSidecarServiceClient(esSidecarConn)

	err = purge.ConfigureManager(
		cerealManager,
		PurgeWorkflowName,
		purge.WithTaskEsSidecarClient(esSidecarClient),
	)
	if err != nil {
		return nil, errors.Wrapf(err, "failed to configure %s workflow", PurgeWorkflowName)
	}

	recurrence, err = rrule.NewRRule(rrule.ROption{
		Freq:     rrule.DAILY,
		Interval: 1,
		Dtstart:  time.Now(),
	})
	if err != nil {
		return nil, errors.Wrapf(err, "could not create recurrence rule for %s", PurgeScheduleName)
	}

	err = purge.CreateOrUpdatePurgeWorkflow(
		timeoutCtx,
		cerealManager,
		PurgeScheduleName,
		PurgeWorkflowName,
		defaultPurgePolicies,
		true,
		recurrence,
	)
	if err != nil {
		return nil, errors.Wrap(err, "failed to create or update purge workflow schedule")
	}

	return purge.NewServer(
		cerealManager,
		PurgeScheduleName,
		PurgeWorkflowName,
		defaultPurgePolicies,
		purge.WithServerEsSidecarClient(esSidecarClient),
	)
}

func setup(ctx context.Context, connFactory *secureconn.Factory, conf config.Compliance,
	esr relaxting.ES2Backend, db *pgdb.DB, cerealManager *cereal.Manager) error {
	var err error
	var conn, mgrConn, secretsConn, authnConn, authzConn *grpc.ClientConn
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
	ingestClient := ingest.NewComplianceIngesterServiceClient(conn)
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
	scanner := scanner.New(mgrClient, nodesClient, db)
	resolver := resolver.New(mgrClient, nodesClient, db, secretsClient)

	err = runner.InitCerealManager(cerealManager, conf.InspecAgent.JobWorkers, ingestClient, scanner, resolver, conf.RemoteInspecVersion)
	if err != nil {
		return errors.Wrap(err, "failed to initialize cereal manager")
	}

	err = cerealManager.Start(ctx)
	if err != nil {
		return errors.Wrap(err, "failed to start cereal manager")
	}
	schedulerServer := scheduler.New(scanner, cerealManager)

	// start polling for jobs with a recurrence schedule that are due to run.
	// this function will sleep for one minute, then query the db for all jobs
	// with recurrence and check if it's time to run the job
	go schedulerServer.PollForJobs(ctx)

	if os.Getenv("RUN_MODE") == "test" {
		logrus.Infof(`Skipping AUTHN client setup due to RUN_MODE env var being set to "test"`)
	} else {
		// get the authn-service connection
		logrus.Debugf("compliance setup, dialing authn-service(%s)", conf.InspecAgent.AuthnTarget)
		authnConn, err = connFactory.DialContext(timeoutCtx, "authn-service", conf.InspecAgent.AuthnTarget,
			grpc.WithBlock())
		if err != nil || authnConn == nil {
			err = errors.New("compliance setup, error grpc dialing to authn aborting...")
			return err
		}
		// get the authn client
		authnClient := authn.NewTokensMgmtServiceClient(authnConn)
		if authnClient == nil {
			logrus.Errorf("serveGrpc got nil for NewTokensMgmtClient: %s", err)
			return err
		}
		// get authz connection
		authzConn, err = connFactory.DialContext(timeoutCtx, "authz-service", fmt.Sprintf("%s:%d", conf.Authz.HostBind, conf.Authz.Port),
			grpc.WithBlock())
		if err != nil || authzConn == nil {
			err = errors.New("compliance setup, error grpc dialing to authz aborting...")
			return err
		}
		// get the authz client
		authzClient := authz.NewPoliciesServiceClient(authzConn)
		if authzClient == nil {
			logrus.Errorf("serveGrpc got nil for NewPoliciesClient: %s", err)
			return err
		}
		// in order to execute scan jobs remotely (i.e. on a different server, reporting back out
		// to automate), we need access to the auth client for a token and the automate fqdn for reporting
		remote.RemoteJobInfo = remote.RemoteJob{
			PoliciesClient:   authzClient,
			TokensMgmtClient: authnClient,
			AutomateFQDN:     conf.InspecAgent.AutomateFQDN,
		}
	}

	SERVICE_STATE = serviceStateStarted
	return nil
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

	cerealManager, err := cereal.NewManager(postgres.NewPostgresBackend(conf.Postgres.ConnectionString))
	if err != nil {
		return err
	}

	defer func() {
		err := cerealManager.Stop()
		if err != nil {
			logrus.WithError(err).Error("could not stop cereal manager")
		}
	}()

	go serveGrpc(ctx, db, connFactory, esr, conf, grpcBinding, statusSrv, cerealManager) // nolint: errcheck

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
