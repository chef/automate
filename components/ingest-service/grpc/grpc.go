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

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/api/interservice/data_lifecycle"
	"github.com/chef/automate/api/interservice/es_sidecar"
	automate_event "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/api/interservice/ingest"
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/backend/elastic"
	"github.com/chef/automate/components/ingest-service/migration"
	"github.com/chef/automate/components/ingest-service/server"
	"github.com/chef/automate/components/ingest-service/serveropts"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	project_update_lib "github.com/chef/automate/lib/authz"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/postgres"
	"github.com/chef/automate/lib/datalifecycle/purge"
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

	var (
		grpcServer = opts.ConnFactory.NewServer()
		migrator   = migration.New(client)
		uri        = fmt.Sprintf("%s:%d", opts.Host, opts.Port)
	)

	log.WithField("uri", uri).Info("Starting gRPC Server")
	conn, err := net.Listen("tcp", uri)
	if err != nil {
		log.WithError(err).Error("TCP listen failed")
		return err
	}

	// Register Status Server
	ingestStatus := server.NewIngestStatus(client, migrator)
	ingest.RegisterIngestStatusServer(grpcServer, ingestStatus)

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

	// Authz Interface
	authzConn, err := opts.ConnFactory.Dial("authz-service", opts.AuthzAddress)
	if err != nil {
		// This should never happen
		log.WithError(err).Error("Failed to create Authz connection")
		return err
	}
	defer authzConn.Close()

	authzProjectsClient := iam_v2.NewProjectsClient(authzConn)

	// event Interface
	eventConn, err := opts.ConnFactory.Dial("event-service", opts.EventAddress)
	if err != nil {
		// This should never happen
		log.WithError(err).Error("Failed to create Event connection")
		return err
	}
	defer eventConn.Close()

	eventServiceClient := automate_event.NewEventServiceClient(eventConn)

	// nodemanager Interface
	nodeMgrConn, err := opts.ConnFactory.Dial("nodemanager-service", opts.NodeManagerAddress)
	if err != nil {
		// This should never happen
		log.WithError(err).Error("Failed to create NodeManager connection")
		return err
	}
	defer nodeMgrConn.Close()

	nodeMgrServiceClient := manager.NewNodeManagerServiceClient(nodeMgrConn)

	// ChefRuns
	chefIngest := server.NewChefIngestServer(client, authzProjectsClient, nodeMgrServiceClient,
		opts.ChefIngestServerConfig)
	ingest.RegisterChefIngesterServer(grpcServer, chefIngest)

	// Pass the chef ingest server to give status about the pipelines
	ingestStatus.SetChefIngestServer(chefIngest)

	// JobSchedulerServer
	pgURL, err := pgURL(opts.PGURL, opts.PGDatabase)
	if err != nil {
		log.WithError(err).Fatal("could not get PG URL")
		return err
	}

	jobManager, err := cereal.NewManager(postgres.NewPostgresBackend(pgURL))
	if err != nil {
		logrus.WithError(err).Fatal("could not create job manager")
		return err
	}
	defer jobManager.Stop()

	esSidecarConn, err := opts.ConnFactory.Dial("es-sidecar-service", opts.EsSidecarAddress)
	if err != nil {
		log.WithError(err).Error("Failed to create connection to es-sidecar-service")
		return err
	}
	defer esSidecarConn.Close()
	esSidecarClient := es_sidecar.NewEsSidecarClient(esSidecarConn)

	err = server.InitializeJobManager(jobManager, client, esSidecarClient)
	if err != nil {
		logrus.WithError(err).Fatal("could not initialize job manager")
		return err
	}

	err = server.MigrateJobsSchedule(context.Background(), jobManager, viper.ConfigFileUsed())
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
	ingest.RegisterJobSchedulerServer(grpcServer, jobSchedulerServer)

	purgeServer, err := purge.NewServer(
		jobManager,
		server.PurgeScheduleName,
		server.PurgeJobName,
		server.DefaultPurgePolicies,
		purge.WithServerEsSidecarClient(esSidecarClient),
	)
	if err != nil {
		logrus.WithError(err).Fatal("could not start start purge server")
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

	if err := projectUpdateManager.Start(context.Background()); err != nil {
		return err
	}
	defer projectUpdateManager.Stop() // nolint: errcheck

	// EventHandler
	eventHandlerServer := server.NewAutomateEventHandlerServer(client, *chefIngest,
		authzProjectsClient, eventServiceClient)
	ingest.RegisterEventHandlerServer(grpcServer, eventHandlerServer)

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
