package server

import (
	"context"
	"net"
	"os"
	"os/signal"
	"path"
	"syscall"

	"github.com/boltdb/bolt"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/airgap"
	"github.com/chef/automate/components/automate-deployment/pkg/converge"
	"github.com/chef/automate/components/automate-deployment/pkg/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/history"
	"github.com/chef/automate/components/automate-deployment/pkg/persistence"
	"github.com/chef/automate/components/automate-deployment/pkg/persistence/boltdb"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/secrets"
	"github.com/chef/automate/lib/stringutils"
	"github.com/chef/automate/lib/tracing"
	"github.com/chef/automate/lib/version"
	grpc_middleware "github.com/grpc-ecosystem/go-grpc-middleware"
	grpc_logrus "github.com/grpc-ecosystem/go-grpc-middleware/logging/logrus"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"
)

const (
	EventInitializeDeploymentService history.EventClass = "StartupInit"
	EventCreateDeployment                               = "StartupCreateDeployment"
	EventGenerateSecret                                 = "GenerateSecret"
)

func openDatabase() (*bolt.DB, error) {

	dbFile := path.Join(DataDir, DBName)

	database, err := bolt.Open(dbFile, 0600, nil)
	if err != nil {
		return nil, err
	}

	return database, err
}

func (s *server) initDeploymentFromDB(context.Context) error {
	err := s.deploymentStore.Initialize()
	if err != nil {
		logrus.WithError(err).Error("could not initialize database")
		return err
	}

	existingDeployment, err := s.deploymentStore.GetDeployment()
	switch err {
	case nil:
		logrus.WithFields(
			logrus.Fields{
				"id":         existingDeployment.ID,
				"created_at": existingDeployment.CreatedAt,
				"deployed":   existingDeployment.Deployed,
			}).Info("Found existing deployment in the database")
	case persistence.ErrDoesNotExist:
		logrus.Info("Creating a deployment as no existing deployment was found in the database")
		existingDeployment, err = deployment.CreateDeployment()
		if err != nil {
			logrus.WithError(err).Error("could not create new deployment")
			return err
		}
		logrus.WithFields(logrus.Fields{
			"id":         existingDeployment.ID,
			"created_at": existingDeployment.CreatedAt,
			"deployed":   existingDeployment.Deployed,
		}).Info("New deployment created")
	default:
		logrus.WithError(err).Error("could not restore deployment from database")
		return err
	}

	err = existingDeployment.InitCA(DataDir)
	if err != nil {
		logrus.WithError(err).Error("Failed to initialize CA for existing deployment")
		return err
	}

	// TODO: set this elsewhere
	existingDeployment.SetTarget(target.NewLocalTarget(airgap.AirgapInUse()))

	// TODO(jaym): This should be on the deployment
	if existingDeployment.Config != nil {
		s.releaseManifestProvider = s.initializeManifestProvider(existingDeployment.Config)
	}

	s.deployment = existingDeployment

	return s.persistDeployment()
}

func (s *server) initSecretStore(ctx context.Context) error {
	var err error

	s.secretStore, err = secrets.NewDefaultSecretStore()
	if err != nil {
		return err
	}

	// Populate the secret store with secrets we need to ensure exist during
	// backup restoration.
	backupSecrets := []secrets.SecretName{
		{Group: "backup-gateway", Name: "access_key"},
		{Group: "backup-gateway", Name: "secret_key"},
	}

	for _, secret := range backupSecrets {
		exists, err := s.secretStore.Exists(secret)
		if err != nil {
			return err
		}

		if !exists {
			err := history.WithTags(
				history.Tag{"secret_group", secret.Group},
				history.Tag{"secret_name", secret.Name},
			).Do(ctx, EventGenerateSecret, "Generating Secret", func(ctx context.Context) error {
				randomBytes, err := secrets.GenerateRandomBytes(64)
				if err != nil {
					return err
				}

				if err = s.secretStore.SetSecret(secret, randomBytes); err != nil {
					return err
				}
				return nil
			})

			if err != nil {
				return err
			}
		}
	}

	return nil
}

func (server *server) initialize(ctx context.Context) error {
	database, err := openDatabase()
	if err != nil {
		return errors.Wrap(err, "could not initiate database")
	}
	server.deploymentStore = boltdb.NewDeploymentStore(database)

	err = server.initDeploymentFromDB(ctx)
	if err != nil {
		server.deploymentStore.Close()
		return errors.Wrap(err, "failed to initialize deployment")
	}

	err = server.initSecretStore(ctx)
	if err != nil {
		server.deploymentStore.Close()

		return errors.Wrap(err, "failed to initialize secret store")
	}

	return nil
}

// StartServer starts the automate deployment gRPC server
func StartServer(config *Config) error {
	address := config.getAddressString()
	setLogrusLevel(config.LogLevel)
	setAndLogProcessState()
	history.RegisterInterceptor(history.NewLogrusInterceptor())

	server := &server{
		serverConfig:         config,
		ensureStatusTimeout:  durationFromSecs(config.EnsureStatusTimeoutSecs, defaultEnsureStatusTimeout),
		ensureStatusInterval: durationFromSecs(config.EnsureStatusIntervalSecs, defaultEnsureStatusInterval),
	}

	err := history.Do(context.Background(), EventInitializeDeploymentService,
		"Initializing Deployment Service", server.initialize)
	if err != nil {
		return err
	}
	defer server.deploymentStore.Close()

	certs, _, err := server.readOrGenDeploymentServiceCerts()
	if err != nil {
		return errors.Wrap(err, "failed to generate TLS certificate")
	}

	server.connFactory = secureconn.NewFactory(*certs, secureconn.WithVersionInfo(
		version.Version,
		version.GitSHA,
	))
	listener, err := net.Listen("tcp", address)
	if err != nil {
		return errors.Wrapf(err, "could not listen on address: %s", address)
	}

	logrusEntry := logrus.NewEntry(logrus.StandardLogger())
	logrusOpts := []grpc_logrus.Option{
		// Don't spam the log with health-check logs
		grpc_logrus.WithDecider(func(m string, _ error) bool {
			return !stringutils.SliceContains([]string{
				// Called by our health-check script
				"/chef.automate.domain.deployment.Deployment/Ping",
				// Called on every chef-automate command for auto-updating
				"/chef.automate.domain.deployment.Deployment/ManifestVersion",
			}, m)
		}),
	}

	logrus.Infof("Starting GRPC automate-deploy service on %s", address)
	grpcServer := server.connFactory.NewServer(
		grpc.StreamInterceptor(grpc_logrus.StreamServerInterceptor(logrusEntry, logrusOpts...)),
		grpc.UnaryInterceptor(grpc_middleware.ChainUnaryServer(
			grpc_logrus.UnaryServerInterceptor(logrusEntry, logrusOpts...),
			tracing.ServerInterceptor(tracing.GlobalTracer()))))

	server.converger = converge.StartConverger()
	err = server.reloadBackupRunner()
	if err != nil {
		return errors.Wrap(err, "failed to load the backup runner")
	}

	// register grpc services
	api.RegisterDeploymentServer(grpcServer, server)
	api.RegisterCertificateAuthorityServer(grpcServer, server)

	reflection.Register(grpcServer)

	convergeIntervalDuration := durationFromSecs(config.ConvergeIntervalSecs, defaultConvergeInterval)

	server.convergeLoop = NewLooper(convergeIntervalDuration, periodicConverger(server, grpcServer))

	err = server.target().UnsetDeploymentServiceReconfigurePending()
	if err != nil {
		return errors.Wrap(err, "failed to unset reconfigure-pending sentinel")
	}

	ch := make(chan os.Signal, 1)
	signal.Notify(ch, syscall.SIGTERM, syscall.SIGINT)
	go func() {
		sig := <-ch
		grpcServer.GracefulStop()
		logrus.WithField("signal", sig).Info("Exiting")
		os.Exit(0)
	}()

	usrChan := make(chan os.Signal, 1)
	signal.Notify(usrChan, syscall.SIGUSR1)
	go func() {
		sig := <-usrChan
		logrus.WithField("signal", sig).Info("Stopping all Chef Automate services")
		go func() {
			err := server.shutItAllDown()
			if err != nil {
				logrus.WithError(err).Error("Failed to shut down Automate from signal handler")
			}
		}()
	}()

	hupChan := make(chan os.Signal, 1)
	signal.Notify(hupChan, syscall.SIGHUP)
	go server.ReconfigureHandler(hupChan, grpcServer)

	server.convergeLoop.Start()
	return grpcServer.Serve(listener)
}
