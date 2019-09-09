package nodemanager

import (
	"context"
	"fmt"
	"net"
	"time"

	"github.com/pkg/errors"
	rrule "github.com/teambition/rrule-go"

	"github.com/chef/automate/api/external/secrets"
	secretsapi "github.com/chef/automate/api/external/secrets"
	eventsapi "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/components/compliance-service/utils/logging"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	managerserver "github.com/chef/automate/components/nodemanager-service/api/manager/server"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
	nodesserver "github.com/chef/automate/components/nodemanager-service/api/nodes/server"
	"github.com/chef/automate/components/nodemanager-service/config"
	"github.com/chef/automate/components/nodemanager-service/managers"
	"github.com/chef/automate/components/nodemanager-service/pgdb"
	"github.com/chef/automate/components/nodemanager-service/polling"

	"github.com/chef/automate/lib/cereal"
	grpccereal "github.com/chef/automate/lib/cereal/grpc"
	"github.com/chef/automate/lib/cereal/patterns"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/version"

	"github.com/chef/automate/lib/tracing"
	"github.com/sirupsen/logrus"
	log "github.com/sirupsen/logrus"
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

func Serve(conf config.Nodemanager, grpcBinding string) error {
	SERVICE_STATE = serviceStateUnknown
	logging.SetLogLevel(conf.Service.LogLevel)
	serviceCerts, err := conf.Service.ReadCerts()
	if err != nil {
		return errors.Wrap(err, "Unable to load service certificates")
	}
	connFactory := secureconn.NewFactory(*serviceCerts, secureconn.WithVersionInfo(
		version.BuildTime,
		version.GitSHA,
	))
	ctx := context.Background()
	SERVICE_STATE = serviceStateStarting

	return serve(ctx, &conf, connFactory)
}

const (
	Awsec2PollingJobName        = "awsec2_polling"
	Awsec2PollingScheduleName   = "awsec2_polling_schedule"
	AzurevmPollingJobName       = "azurevm_polling"
	AzurevmPollingScheduleName  = "azurevm_polling_schedule"
	ManagersPollingJobName      = "managers_polling"
	ManagersPollingScheduleName = "managers_polling_schedule"
)

// types
type CheckAWSNodesTask struct {
	db            *pgdb.DB
	secretsClient secrets.SecretsServiceClient
	eventsClient  eventsapi.EventServiceClient
}

type CheckAzureNodesTask struct {
	db            *pgdb.DB
	secretsClient secrets.SecretsServiceClient
}

type CheckManagersTask struct {
	db            *pgdb.DB
	secretsClient secrets.SecretsServiceClient
}

func serve(ctx context.Context, config *config.Nodemanager, connFactory *secureconn.Factory) error {
	log.Infof("getting db connection")
	db, err := pgdb.New(&config.Postgres)
	if err != nil {
		log.WithError(err).Error("Creating postgres connection")
		return err
	}
	uri := fmt.Sprintf("%s:%d", config.Service.HostBind, config.Service.Port)
	log.WithFields(log.Fields{"uri": uri}).Info("Starting nodemanager gRPC Server")
	conn, err := net.Listen("tcp", uri)
	if err != nil {
		log.WithError(err).Error("TCP listen failed")
		return err
	}
	config.Secrets.Endpoint = fmt.Sprintf("%s:%d", config.Secrets.HostBind, config.Secrets.Port)

	grpcServer := newGRPCServer(db, connFactory, config)

	timeoutCtx, cancel := context.WithTimeout(ctx, 15*time.Second)
	defer cancel()

	// Set the secrets service client
	log.Infof("nodemanager setup, dialing secrets-service(%s)", config.Secrets.Endpoint)
	secretsConn, err := connFactory.DialContext(timeoutCtx, "secrets-service", config.Secrets.Endpoint, grpc.WithBlock())
	if err != nil {
		err = errors.New("nodemanager setup, error grpc dialing to secrets aborting...")
		return err
	}
	log.Debugf("nodemanager setup, getting a secrets service client")
	secretsClient := secretsapi.NewSecretsServiceClient(secretsConn)
	if secretsClient == nil {
		return fmt.Errorf("nodemanager setup, could not obtain secrets service client")
	}

	// Set the events service client
	log.Infof("nodemanager setup, getting an automate-event service client %s", config.EventConfig.Endpoint)
	eventConn, err := connFactory.DialContext(timeoutCtx, "event-service", config.EventConfig.Endpoint, grpc.WithBlock())
	if err != nil {
		err = errors.New("nodemanager setup, error grpc dialing to event-service aborting...")
		return err
	}
	log.Debug("nodemanager setup, getting a events service client")
	eventClient := eventsapi.NewEventServiceClient(eventConn)
	if eventClient == nil {
		return fmt.Errorf("nodemanager setup, could not obtain automate events service client")
	}

	// Set the cereal service client
	log.Infof("nodemanager setup, getting a cereal service client %s", config.Cereal.Endpoint)
	cerealConn, err := connFactory.DialContext(timeoutCtx, "cereal-service", config.Cereal.Endpoint, grpc.WithBlock())
	if err != nil {
		err = errors.New("nodemanager setup, error grpc dialing to cereal-service aborting...")
		return err
	}
	// LATER: look at whatever Dan did for picking the domain
	cerealBackend := grpccereal.NewGrpcBackendFromConn("nodemanager", cerealConn)

	// Set up a cereal (workflow) manager for managing nodemanager workflows.
	cerealManager, err := cereal.NewManager(cerealBackend)
	if err != nil {
		log.Errorf("could not create cereal manager: %s", err)
	}
	defer cerealManager.Stop() //nolint:errcheck

	// Prelude to initializing the job manager: register task executors for the nodemanger polling jobs
	err = cerealManager.RegisterTaskExecutor(Awsec2PollingJobName,
		&CheckAWSNodesTask{db: db, secretsClient: secretsClient, eventsClient: eventClient},
		cereal.TaskExecutorOpts{})
	if err != nil {
		return err
	}

	err = cerealManager.RegisterTaskExecutor(AzurevmPollingJobName,
		&CheckAzureNodesTask{db: db, secretsClient: secretsClient}, cereal.TaskExecutorOpts{})
	if err != nil {
		return err
	}

	err = cerealManager.RegisterTaskExecutor(ManagersPollingJobName,
		&CheckManagersTask{db: db, secretsClient: secretsClient}, cereal.TaskExecutorOpts{})
	if err != nil {
		return err
	}

	// Prelude to initializing the cereal manager: register workflow executors to run tasks
	for _, jobName := range []string{Awsec2PollingJobName, AzurevmPollingJobName, ManagersPollingJobName} {
		err = cerealManager.RegisterWorkflowExecutor(jobName, patterns.NewSingleTaskWorkflowExecutor(jobName, false))
		if err != nil {
			return errors.Wrapf(err, "failed to register workflow for %q", jobName)
		}
	}

	kindsOfChecks := [3]string{"aws", "azure", "manager"}
	var pollInterval int
	var jobName string
	var scheduleName string

	// Prelude to initializing the cereal manager: set up recurrence rules and schedules.
	for _, k := range kindsOfChecks {
		switch checkType := k; checkType {
		case "aws":
			pollInterval = config.AwsEc2PollIntervalMinutes
			jobName = Awsec2PollingJobName
			scheduleName = Awsec2PollingScheduleName
		case "azure":
			pollInterval = config.AzureVMPollIntervalMinutes
			jobName = AzurevmPollingJobName
			scheduleName = AzurevmPollingScheduleName
		case "manager":
			// The default for the manager check is *not* set in the config. 120 was the default in the pre-cereal code.
			pollInterval = 120
			jobName = ManagersPollingJobName
			scheduleName = ManagersPollingScheduleName
		default:
		}

		// Create rrule.
		rule, err := rrule.NewRRule(rrule.ROption{
			Freq:     rrule.MINUTELY,
			Interval: pollInterval,
			Dtstart:  time.Now(),
		})
		if err != nil {
			return errors.Wrapf(err, "Could not create recurrence rule for nodemanager workflow %s", Awsec2PollingJobName)
		}

		// Set up workflow schedule.
		err = createOrUpdateWorkflowSchedule(cerealManager, scheduleName, jobName, rule)
		if err != nil {
			return err
		}

	}

	// The cereal manager has been initialized and can (finally) be started.
	err = cerealManager.Start(context.Background())
	if err != nil {
		logrus.WithError(err).Fatal("could not start cereal manager")
	}

	_, err = managers.CreateManualNodeManager(db)
	if err != nil {
		err = errors.Wrap(err, "Couldn't create Automate manager")
		return err
	}
	SERVICE_STATE = serviceStateStarted
	return grpcServer.Serve(conn)
}

func (t *CheckAWSNodesTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	return nil, polling.QueryAwsEc2InstanceStates(ctx, t.db, t.secretsClient, t.eventsClient)
}

func (t *CheckAzureNodesTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	return nil, polling.QueryAzureVMInstanceStates(ctx, t.db, t.secretsClient)
}

func (t *CheckManagersTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	return nil, polling.CheckManagersStatuses(ctx, t.db, t.secretsClient)
}

func newGRPCServer(db *pgdb.DB, connFactory *secureconn.Factory, config *config.Nodemanager) *grpc.Server {
	s := connFactory.NewServer(tracing.GlobalServerInterceptor())

	nodes.RegisterNodesServiceServer(s, nodesserver.New(db, connFactory, config.Secrets.Endpoint))
	manager.RegisterNodeManagerServiceServer(s, managerserver.New(db, connFactory, config.Secrets.Endpoint))

	healthServer := health.NewService()
	health.RegisterHealthServer(s, healthServer)

	reflection.Register(s)

	return s
}

func createOrUpdateWorkflowSchedule(cerealManager *cereal.Manager, scheduleName string, jobName string, rule *rrule.RRule) error {
	err := cerealManager.CreateWorkflowSchedule(context.Background(), scheduleName, jobName, nil, true, rule)
	if err != nil {
		if err == cereal.ErrWorkflowScheduleExists {
			log.Infof("nodemanager workflow schedule %s already exists, not migrating", scheduleName)
			// If the schedule exists, make sure the rrule is up-to-date.
			schedule, err := cerealManager.GetWorkflowScheduleByName(context.Background(), scheduleName, jobName)
			if err != nil {
				return errors.Wrapf(err, "failed to get scheduled workflow %s from cereal manager", scheduleName)
			}
			scheduledRule, err := schedule.GetRRule()
			if err != nil {
				return errors.Wrapf(err, "unable to get rrule for scheduled workflow %s", scheduleName)
			}
			if scheduledRule != rule {
				err = cerealManager.UpdateWorkflowScheduleByName(context.Background(), scheduleName, jobName, cereal.UpdateRecurrence(rule))
				if err != nil {
					return errors.Wrapf(err, "unable to update recurrence rule for scheduled workflow %s", scheduleName)
				}
			}
		} else {
			return errors.Wrapf(err, "Could not continue creating workflow schedule %s", scheduleName)
		}
	}
	return nil
}
