package nodemanager

import (
	"context"
	"fmt"
	"net"
	"time"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	rrule "github.com/teambition/rrule-go"
	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"

	"github.com/chef/automate/api/external/secrets"
	secretsapi "github.com/chef/automate/api/external/secrets"
	eventsapi "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	"github.com/chef/automate/components/compliance-service/utils/logging"
	"github.com/chef/automate/components/nodemanager-service/config"
	"github.com/chef/automate/components/nodemanager-service/managers"
	"github.com/chef/automate/components/nodemanager-service/pgdb"
	"github.com/chef/automate/components/nodemanager-service/polling"
	managerserver "github.com/chef/automate/components/nodemanager-service/server/manager"
	nodesserver "github.com/chef/automate/components/nodemanager-service/server/nodes"

	"github.com/chef/automate/lib/cereal"
	grpccereal "github.com/chef/automate/lib/cereal/grpc"
	"github.com/chef/automate/lib/cereal/patterns"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/version"

	"github.com/chef/automate/api/interservice/authz"
	project_update_lib "github.com/chef/automate/lib/authz"
	"github.com/chef/automate/lib/tracing"
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

var (
	AwsEc2PollingWorkflowName = cereal.NewWorkflowName("awsec2_polling")
	Awsec2PollingScheduleName = "awsec2_polling_schedule"

	AzureVMPollingWorkflowName = cereal.NewWorkflowName("azurevm_polling")
	AzureVMPollingScheduleName = "azurevm_polling_schedule"

	ManagersPollingWorkflowName = cereal.NewWorkflowName("managers_polling")
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
	log.Debug("getting db connection")
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
	if !config.Cereal.Skip {
		log.Infof("nodemanager setup, getting a cereal service client %s", config.Cereal.Endpoint)
		cerealConn, err := connFactory.DialContext(timeoutCtx, "cereal-service", config.Cereal.Endpoint, grpc.WithBlock())
		if err != nil {
			err = errors.New("nodemanager setup, error grpc dialing to cereal-service aborting...")
			return err
		}
		cerealBackend := grpccereal.NewGrpcBackendFromConn("nodemanager-service", cerealConn)

		// Set up a cereal (workflow) manager for managing nodemanager workflows.
		cerealManager, err := cereal.NewManager(cerealBackend)
		if err != nil {
			return errors.Wrap(err, "could not create cereal manager")
		}
		defer cerealManager.Stop() //nolint:errcheck

		// Prelude to initializing the job manager: register task executors for the nodemanger polling jobs
		err = patterns.RegisterSingleTaskWorkflowExecutor(
			cerealManager,
			AwsEc2PollingWorkflowName,
			false,
			&CheckAWSNodesTask{db: db, secretsClient: secretsClient, eventsClient: eventClient},
			cereal.TaskExecutorOpts{})
		if err != nil {
			return errors.Wrapf(err, "failed to register AWS polling workflow")
		}

		err = patterns.RegisterSingleTaskWorkflowExecutor(
			cerealManager,
			AzureVMPollingWorkflowName,
			false,
			&CheckAzureNodesTask{db: db, secretsClient: secretsClient},
			cereal.TaskExecutorOpts{})
		if err != nil {
			return errors.Wrapf(err, "failed to register Azure polling workflow")
		}

		err = patterns.RegisterSingleTaskWorkflowExecutor(
			cerealManager,
			ManagersPollingWorkflowName,
			false,
			&CheckManagersTask{db: db, secretsClient: secretsClient},
			cereal.TaskExecutorOpts{})
		if err != nil {
			return errors.Wrapf(err, "failed to register managers polling workflow")
		}

		// Prelude to initializing the cereal manager: set up recurrence rules and schedules.
		kindsOfChecks := [3]string{"aws", "azure", "manager"}
		for _, k := range kindsOfChecks {
			var pollInterval int
			var workflowName cereal.WorkflowName
			var scheduleName string

			switch checkType := k; checkType {
			case "aws":
				pollInterval = config.AwsEc2PollIntervalMinutes
				workflowName = AwsEc2PollingWorkflowName
				scheduleName = Awsec2PollingScheduleName
			case "azure":
				pollInterval = config.AzureVMPollIntervalMinutes
				workflowName = AzureVMPollingWorkflowName
				scheduleName = AzureVMPollingScheduleName
			case "manager":
				// The default for the manager check is *not* set in the config. 120 was the default in the pre-cereal code.
				pollInterval = 120
				workflowName = ManagersPollingWorkflowName
				scheduleName = ManagersPollingScheduleName
			default:
				panic("Unable to get data to set up nodemanager's scheduled workflows")
			}

			// Create recurrence role.
			rule, err := rrule.NewRRule(rrule.ROption{
				Freq:     rrule.MINUTELY,
				Interval: pollInterval,
				Dtstart:  time.Now(),
			})
			if err != nil {
				return errors.Wrapf(err, "could not create recurrence rule for nodemanager workflow %s", scheduleName)
			}

			// Set up workflow schedule.
			err = createOrUpdateWorkflowSchedule(cerealManager, scheduleName, workflowName, rule)
			if err != nil {
				return err
			}

		}

		// The cereal manager has been initialized and can (finally) be started.
		err = cerealManager.Start(context.Background())
		if err != nil {
			log.WithError(err).Fatal("could not start cereal manager")
		}

		// Authz Interface
		authzConn, err := connFactory.DialContext(timeoutCtx, "authz-service", config.Authz.Endpoint, grpc.WithBlock())
		if err != nil {
			// This should never happen
			log.WithError(err).Error("Failed to create Authz connection")
			return err
		}
		defer authzConn.Close() // nolint: errcheck

		authzProjectsClient := authz.NewProjectsServiceClient(authzConn)

		projectUpdateManager, err := createProjectUpdateCerealManager(connFactory, config.Cereal.Endpoint)
		if err != nil {
			return err
		}

		// deprecated stuff
		err = project_update_lib.RegisterTaskExecutors(projectUpdateManager, "nodemanager",
			db, authzProjectsClient)
		if err != nil {
			return err
		}

		err = project_update_lib.RegisterSerialTaskExecutors(projectUpdateManager, "nodemanager",
			db, authzProjectsClient)
		if err != nil {
			return err
		}

		if err := projectUpdateManager.Start(context.Background()); err != nil {
			return err
		}
		defer projectUpdateManager.Stop() // nolint: errcheck
	}

	_, err = managers.CreateManualNodeManager(db)
	if err != nil {
		err = errors.Wrap(err, "couldn't create Automate manager")
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

func createOrUpdateWorkflowSchedule(cerealManager *cereal.Manager, scheduleName string, workflowName cereal.WorkflowName, rule *rrule.RRule) error {
	err := cerealManager.CreateWorkflowSchedule(context.Background(), scheduleName, workflowName, nil, true, rule)

	if err == nil {
		return nil
	}

	if err == cereal.ErrWorkflowScheduleExists {
		log.Infof("nodemanager workflow schedule %s already exists, not creating", scheduleName)
		// If the schedule exists, make sure the rrule is up-to-date.
		schedule, err := cerealManager.GetWorkflowScheduleByName(context.Background(), scheduleName, workflowName)
		if err != nil {
			return errors.Wrapf(err, "failed to get scheduled workflow %s from cereal manager", scheduleName)
		}
		scheduledRule, err := schedule.GetRRule()
		if err != nil {
			return errors.Wrapf(err, "unable to get rrule for scheduled workflow %s", scheduleName)
		}
		if scheduledRule != rule {
			err = cerealManager.UpdateWorkflowScheduleByName(context.Background(), scheduleName, workflowName, cereal.UpdateRecurrence(rule))
			if err != nil {
				return errors.Wrapf(err, "unable to update recurrence rule for scheduled workflow %s", scheduleName)
			}
		}
	} else {
		return errors.Wrapf(err, "could not continue creating workflow schedule %s", scheduleName)
	}
	return nil // make the linter happy
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
