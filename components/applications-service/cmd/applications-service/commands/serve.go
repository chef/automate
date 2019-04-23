package commands

import (
	"crypto/tls"
	"fmt"
	"net/http"

	"github.com/prometheus/client_golang/prometheus/promhttp"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"

	"github.com/chef/automate/components/applications-service/pkg/config"
	"github.com/chef/automate/components/applications-service/pkg/grpc"
	"github.com/chef/automate/components/applications-service/pkg/nats"
	"github.com/chef/automate/components/applications-service/pkg/storage/postgres"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/platform"
	"github.com/chef/automate/lib/version"
)

var serveCmd = &cobra.Command{
	Use:   "serve",
	Short: "Launches applications services",
	RunE: func(cmd *cobra.Command, args []string) error {
		conf, err := configFromViper()
		if err != nil {
			log.WithFields(log.Fields{
				"error": err.Error(),
			}).Fatal("Failed to load config")
		}
		conf.Service.SetLogLevel()
		certs, err := conf.ReadCerts()
		if err != nil {
			log.WithFields(log.Fields{
				"error": err.Error(),
			}).Fatal("Failed to load SSL key/cert files")
		}
		connFactory := secureconn.NewFactory(*certs,
			secureconn.WithVersionInfo(version.Version, version.GitSHA))

		// Storage client (Postgres)
		dbClient, err := postgres.New(&conf.Postgres)
		if err != nil {
			log.WithFields(log.Fields{
				"error": err.Error(),
			}).Fatal("Failed to create postgres client")
		}
		conf.SetStorage(dbClient)

		go func() {
			natsClient := nats.NewDefaults(
				fmt.Sprintf("nats://%s:%d", conf.Events.Host, conf.Events.Port),
				conf.Events.ClusterID,
				*conf.TLSConfig,
			)

			// Trigger NATS Subscription
			err := natsClient.ConnectAndSubscribe()
			if err != nil {
				log.WithError(err).Fatal("could not connect to nats-streaming")
			}

			// Receive digested messages from the event channel and
			// send them to the datastore for processing
			// TODO @afiune Move this logic into an ingestion pipeline
			for {
				select {
				case event := <-natsClient.HabServiceEventCh:
					err := dbClient.IngestHabEvent(event)
					if err != nil {
						log.WithFields(log.Fields{
							"error": err.Error(),
						}).Error("Unable to ingest habitat event")
					}
				}
			}
		}()

		go func() {
			// TODO (dan): this is getting long and should move out of this file
			r := http.NewServeMux()
			r.Handle("/metrics", promhttp.Handler())

			httpServer := http.Server{
				Addr:    fmt.Sprintf("%s:%d", conf.Service.Host, conf.Service.MetricsPort),
				Handler: r,
				TLSConfig: &tls.Config{
					Certificates:             []tls.Certificate{*certs.ServiceKeyPair},
					ClientCAs:                certs.NewCertPool(),
					ClientAuth:               tls.RequireAndVerifyClientCert,
					PreferServerCipherSuites: true,
					MinVersion:               tls.VersionTLS12,
					CipherSuites:             secureconn.DefaultCipherSuites(),
				},
			}

			log.Fatal(httpServer.ListenAndServeTLS("", ""))
		}()

		// GRPC Server
		return grpc.Spawn(conf, connFactory)
	},
}

func configFromViper() (*config.Applications, error) {
	cfg := &config.Applications{}
	if err := viper.Unmarshal(cfg); err != nil {
		log.WithFields(log.Fields{
			"error": err.Error(),
		}).Fatal("Failed to marshal config options to server config")
	}
	cfg.FixupRelativeTLSPaths(viper.ConfigFileUsed())

	if cfg.Postgres.URI == "" {
		var err error
		cfg.Postgres.URI, err = platform.PGURIFromEnvironment(cfg.Postgres.Database)
		if err != nil {
			log.WithError(err).Error("Failed to get pg uri")
			return nil, err
		}
	}

	return cfg, nil
}

func init() {
	RootCmd.AddCommand(serveCmd)
}
