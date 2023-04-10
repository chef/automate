package grpc

import (
	"context"
	"fmt"
	"net"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/viper"
	"google.golang.org/grpc/reflection"

	"github.com/chef/automate/api/interservice/authz"
	cfgmgmt "github.com/chef/automate/api/interservice/cfgmgmt/service"
	"github.com/chef/automate/api/interservice/data_lifecycle"
	"github.com/chef/automate/api/interservice/es_sidecar"
	"github.com/chef/automate/api/interservice/event_feed"
	"github.com/chef/automate/api/interservice/ingest"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/backend/elastic"
	"github.com/chef/automate/components/ingest-service/migration"
	"github.com/chef/automate/components/ingest-service/pipeline"
	"github.com/chef/automate/components/ingest-service/server"
	"github.com/chef/automate/components/ingest-service/serveropts"
	project_update_lib "github.com/chef/automate/lib/authz"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/postgres"
	"github.com/chef/automate/lib/datalifecycle/purge"
	libdb "github.com/chef/automate/lib/db"
	"github.com/chef/automate/lib/grpc/secureconn"
	platform_config "github.com/chef/automate/lib/platform/config"
)

// Spawn starts a gRPC Server listening on the provided host and port,
// it uses the backend URL to initialize the Pipelines
//
// You can start a server in a goroutine to work independently like:
// ```
// go server.Spawn("localhost", "2193", "elasticsearch:9200")
// ```
//
// Maybe even spawn multiple servers
func Spawn(opts *serveropts.Opts) error {
	var client backend.Client

	// Initialize the backend client
	client, err := elastic.New(opts.ElasticSearchUrl)

	if err != nil {
		log.WithFields(log.Fields{
			"url":   opts.ElasticSearchUrl,
			"error": err.Error(),
		}).Error("could not connect to elasticsearch")
		return err
	}
	grpcServer := opts.ConnFactory.NewServer()

	uri := fmt.Sprintf("%s:%d", opts.Host, opts.Port)
	log.WithField("uri", uri).Info("Starting gRPC Server")
	conn, err := net.Listen("tcp", uri)
	if err != nil {
		log.WithError(err).Error("TCP listen failed")
		return err
	}

	pgURL, err := pgURL(opts.PGURL, opts.PGDatabase)
	if err != nil {
		log.WithError(err).Fatal("could not get PG URL")
		return err
	}

	db, err := libdb.PGOpen(pgURL)
	if err != nil {
		return errors.Wrapf(err, "Failed to open database with uri: %s", pgURL)
	}

	// Authz Interface
	authzConn, err := opts.ConnFactory.Dial("authz-service", opts.AuthzAddress)
	if err != nil {
		log.WithError(err).Error("Failed checking if a migration is running")
		return err
	}

	authzProjectsClient := authz.NewProjectsServiceClient(authzConn)

	// event feed Interface
	eventFeedConn, err := opts.ConnFactory.Dial("event-feed-service", opts.EventFeedAddress)
	if err != nil {
		// This should never happen
		log.WithError(err).Error("Failed to create Event Feed connection")
		return err
	}
	defer eventFeedConn.Close() // nolint: errcheck

	eventFeedServiceClient := event_feed.NewEventFeedServiceClient(eventFeedConn)

	// Need to ensure the event feed service is ready for requests.
	_, err = eventFeedServiceClient.GetFeed(context.Background(), &event_feed.FeedRequest{
		Size: 100,
	})
	if err != nil {
		log.WithError(err).Error("Event Feed is not ready")
		return err
	}

	// nodemanager Interface
	nodeMgrConn, err := opts.ConnFactory.Dial("nodemanager-service", opts.NodeManagerAddress)
	if err != nil {
		// This should never happen
		log.WithError(err).Error("Failed to create NodeManager connection")
		return err
	}
	defer nodeMgrConn.Close() // nolint: errcheck

	nodeMgrServiceClient := manager.NewNodeManagerServiceClient(nodeMgrConn)
	nodesServiceClient := nodes.NewNodesServiceClient(nodeMgrConn)

	configMgmtConn, err := opts.ConnFactory.Dial("config-mgmt-service", opts.ConfigMgmtAddress)
	if err != nil {
		log.WithError(err).WithFields(log.Fields{"config_mgmt_address": opts.ConfigMgmtAddress}).Error("Failed to connect to config-mgmt-service")
		return err
	}
	configMgmtClient := cfgmgmt.NewCfgMgmtServiceClient(configMgmtConn)

	chefActionPipeline := pipeline.NewChefActionPipeline(client, authzProjectsClient,
		configMgmtClient, eventFeedServiceClient,
		opts.ChefIngestServerConfig.MaxNumberOfBundledActionMsgs,
		opts.ChefIngestServerConfig.MessageBufferSize)

	chefRunPipeline := pipeline.NewChefRunPipeline(client, authzProjectsClient,
		nodeMgrServiceClient, opts.ChefIngestServerConfig.ChefIngestRunPipelineConfig,
		opts.ChefIngestServerConfig.MessageBufferSize)

	migrator := migration.New(client, chefActionPipeline, db)

	// Register Status Server
	ingestStatus := server.NewIngestStatus(client, migrator)
	ingest.RegisterIngestStatusServiceServer(grpcServer, ingestStatus)

	// Initialize elasticsearch indices or trigger a migration
	//
	// This initialization will happen inside a goroutine so that we can then
	// report the health of the system. We might have to do the same for the
	// migration tasks.
	migrationNeeded, err := migrator.MigrationNeeded()
	if err != nil {
		log.WithError(err).Error("Failed checking for migration")
		return err
	}

	migrationRunning, err := migrator.MigrationRunning()
	if err != nil {
		log.WithError(err).Error("Failed checking if a migration is running")
		return err
	}

	if !migrationRunning {
		if migrationNeeded {
			err = migrator.Start()
			if err != nil {
				log.WithError(err).Error("Failed starting a migration")
				return err
			}
		} else {
			migrator.MarkUnneeded()
			err = client.InitializeStore(context.Background())
			if err != nil {
				log.WithError(err).Error("Failed initializing elasticsearch")
				return err
			}
		}
	} else {
		log.Warnf("HA Frontend: migration is currently running on another service. "+
			"If this is an error, remove the lock from postgresql with the "+
			"'SELECT pg_advisory_unlock(%d);' command inside the chef_ingest_service database.", migration.PgMigrationLockID)
	}

	// ChefRuns
	chefIngest := server.NewChefIngestServer(client, authzProjectsClient, nodeMgrServiceClient,
		nodesServiceClient, chefActionPipeline, chefRunPipeline)

	ingest.RegisterChefIngesterServiceServer(grpcServer, chefIngest)

	// Pass the chef ingest server to give status about the pipelines
	ingestStatus.SetChefIngestServer(chefIngest)

	jobManager, err := cereal.NewManager(postgres.NewPostgresBackend(pgURL))
	if err != nil {
		logrus.WithError(err).Fatal("could not create job manager")
		return err
	}
	defer jobManager.Stop() // nolint: errcheck

	esSidecarConn, err := opts.ConnFactory.Dial("es-sidecar-service", opts.EsSidecarAddress)
	if err != nil {
		log.WithError(err).Error("Failed to create connection to es-sidecar-service")
		return err
	}
	defer esSidecarConn.Close() // nolint: errcheck
	esSidecarClient := es_sidecar.NewEsSidecarServiceClient(esSidecarConn)

	err = server.InitializeJobManager(jobManager, client, esSidecarClient, nodeMgrServiceClient,
		nodesServiceClient)
	if err != nil {
		logrus.WithError(err).Fatal("could not initialize job manager")
		return err
	}

	err = server.MigrateJobsSchedule(context.Background(), jobManager, viper.ConfigFileUsed(), opts.Jobs)
	if err != nil {
		logrus.WithError(err).Fatal("could not migrate old job schedules")
		return err
	}

	err = server.ConfigurePurge(jobManager, opts)
	if err != nil {
		logrus.WithError(err).Fatal("could not configure purge policies")
		return err
	}

	err = jobManager.Start(context.Background())
	if err != nil {
		logrus.WithError(err).Fatal("could not start job manager")
		return err
	}

	jobSchedulerServer := server.NewJobSchedulerServer(client, jobManager)
	ingest.RegisterJobSchedulerServiceServer(grpcServer, jobSchedulerServer)

	purgeServer, err := purge.NewServer(
		jobManager,
		server.PurgeScheduleName,
		server.PurgeWorkflowName,
		server.DefaultPurgePolicies,
		purge.WithServerEsSidecarClient(esSidecarClient),
	)
	if err != nil {
		logrus.WithError(err).Fatal("could not start purge server")
		return err
	}

	data_lifecycle.RegisterPurgeServer(grpcServer, purgeServer)

	projectUpdateManager, err := createProjectUpdateCerealManager(opts.ConnFactory, opts.CerealAddress)
	if err != nil {
		return err
	}

	err = project_update_lib.RegisterTaskExecutors(projectUpdateManager, "ingest", client, authzProjectsClient)
	if err != nil {
		return err
	}

	err = project_update_lib.RegisterSerialTaskExecutors(projectUpdateManager, "ingest", client, authzProjectsClient)
	if err != nil {
		return err
	}

	if err := projectUpdateManager.Start(context.Background()); err != nil {
		return err
	}
	defer projectUpdateManager.Stop() // nolint: errcheck

	// EventHandler
	eventHandlerServer := server.NewAutomateEventHandlerServer(client, *chefIngest,
		authzProjectsClient)
	ingest.RegisterEventHandlerServiceServer(grpcServer, eventHandlerServer)

	// Register reflection service on gRPC server.
	reflection.Register(grpcServer)

	return grpcServer.Serve(conn)
}

func pgURL(pgURL string, pgDBName string) (string, error) {
	if pgURL == "" {
		var err error
		pgURL, err = platform_config.PGURIFromEnvironment(pgDBName)
		if err != nil {
			return "", errors.Wrap(err, "Failed to get pg uri")
		}
	}
	return pgURL, nil
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
