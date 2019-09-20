package commands

import (
	"context"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/data-feed-service/config"
	"github.com/chef/automate/components/data-feed-service/dao"
	"github.com/chef/automate/components/data-feed-service/server"
	"github.com/chef/automate/components/data-feed-service/service"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tracing"
)

var serveCmd = &cobra.Command{
	Use:   "serve",
	Short: "Start the data feed gRPC server",
	RunE: func(cmd *cobra.Command, args []string) error {
		log.Info("Starting Data Feed Service")
		closer, err := tracing.NewGlobalTracer("data-feed-service")
		if err != nil {
			log.WithError(err).Warn("Failed to start tracer for data-feed-service")
		}
		if closer != nil {
			defer tracing.CloseQuietly(closer)
		}

		cfg, err := config.Configure()
		if err != nil {
			return errors.Wrap(err, "failed to configure data-feed-service")
		}

		connFactory := secureconn.NewFactory(*cfg.ServiceCerts)

		// create backend
		db, err := dao.New(&cfg.PostgresConfig)
		if err != nil {
			log.WithFields(log.Fields{"error": err}).Fatal("Creating postgres connection")
			return errors.Wrap(err, "failed to configure database")
		}

		err = service.Start(cfg, connFactory, db)
		if err != nil {
			return errors.Wrap(err, "failed to start service")
		}

		err = server.StartGRPC(context.Background(), cfg, connFactory, db)
		if err != nil {
			return errors.Wrap(err, "failed to start GRPC server")
		}

		return nil
	},
}
