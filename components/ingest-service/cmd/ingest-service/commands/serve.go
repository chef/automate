package commands

import (
	"fmt"
	"os"

	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"

	"github.com/chef/automate/components/ingest-service/grpc"
	"github.com/chef/automate/components/ingest-service/rest"
	"github.com/chef/automate/components/ingest-service/serveropts"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/certs"
	"github.com/chef/automate/lib/tracing"
	"github.com/chef/automate/lib/version"
)

const devModeEnvVar = "CHEF_DEV_ENVIRONMENT"

var serveCmd = &cobra.Command{
	Use:   "serve",
	Short: "Launches the ingest service on https://localhost:2192",
	Run: func(cmd *cobra.Command, args []string) {
		conf := readCliParams()

		conf.SetLogLevel()
		logrus.Info("Starting Ingest Services")

		closer, err := tracing.NewGlobalTracer("ingest-service")
		if err == nil {
			defer tracing.CloseQuietly(closer)
		}

		// construct GRPC endpoint for gateway
		endpoint := fmt.Sprintf("%s:%d", conf.Host, conf.Port)

		// Spawn a gRPC Client in a goroutine
		//
		// This goroutine will be attach to the main routine that
		// will be the gRPC Server, as long as the Server is up,
		// the client should also be up.
		//
		// TODO: Figure out how to respawn if client crashes?
		if os.Getenv(devModeEnvVar) == "true" {
			go rest.Spawn(endpoint, conf)
		}

		// Start the gRPC Server
		grpc.Spawn(conf)
	},
}

func readCliParams() *serveropts.Opts {
	tlsConfig := certs.TLSConfig{
		CertPath:       viper.GetString("cert"),
		KeyPath:        viper.GetString("key"),
		RootCACertPath: viper.GetString("root-cert"),
	}
	serviceCerts, err := tlsConfig.ReadCerts()
	if err != nil {
		logrus.WithFields(logrus.Fields{
			"error": err.Error(),
		}).Fatal("Failed to load SSL key/cert files")
	}

	// create the grpc connection factory
	factory := secureconn.NewFactory(*serviceCerts, secureconn.WithVersionInfo(
		version.Version,
		version.GitSHA,
	))

	return &serveropts.Opts{
		Host:                          viper.GetString("host"),
		Port:                          viper.GetInt("port"),
		RestPort:                      viper.GetInt("rest-port"),
		ElasticSearchUrl:              viper.GetString("elasticsearch-url"),
		EsSidecarAddress:              viper.GetString("es-sidecar-address"),
		AuthzAddress:                  viper.GetString("authz-address"),
		EventAddress:                  viper.GetString("event-address"),
		NodeManagerAddress:            viper.GetString("nodemanager-address"),
		LogLevel:                      viper.GetString("log-level"),
		PurgeConvergeHistoryAfterDays: int32(viper.GetInt("converge-history-days")),
		PurgeActionsAfterDays:         int32(viper.GetInt("actions-days")),
		MaxNumberOfBundledRunMsgs:     viper.GetInt("max-number-of-bundled-run-msgs"),
		MaxNumberOfBundledActionMsgs:  viper.GetInt("max-number-of-bundled-action-msgs"),
		NumberOfRunMsgsTransformers:   viper.GetInt("number-of-run-msgs-transformers"),
		ConnFactory:                   factory,
	}
}

func init() {
	RootCmd.AddCommand(serveCmd)
	serveCmd.Flags().String("host", "localhost", "hostname or IP that we bind to")
	serveCmd.Flags().String("log-level", "info", "the level of logging")
	serveCmd.Flags().Int("port", 2191, "Port where the gRPC Server will be listening")
	serveCmd.Flags().Int("rest-port", 2192, "Port where the REST Server will be listening")
	serveCmd.Flags().String("elasticsearch-url", "http://localhost:9200", "Url to ElasticSearch (<protocol>://domain:<port>)/")
	serveCmd.Flags().String("es-sidecar-address", "localhost:10390", "address of es sidecar (domain:<port>)")
	serveCmd.Flags().String("authz-address", "localhost:10130", "address of authz (domain:<port>)")
	serveCmd.Flags().String("event-address", "localhost:10132", "address of event (domain:<port>)")
	serveCmd.Flags().String("nodemanager-address", "localhost:10120", "address of nodemanager (domain:<port>)")
	serveCmd.Flags().Int32("converge-history-days", -1, "Number of days to keep converge history for. A number less than or equal to 0 means data should never be deleted")
	serveCmd.Flags().Int32("actions-days", -1, "Number of days to keep actions for. A number less than or equal to 0 means data should never be deleted")
	serveCmd.Flags().Int("max-number-of-bundled-run-msgs", 2500, "The maximum number of run messages to bundle togeter during ingestion")
	serveCmd.Flags().Int("max-number-of-bundled-action-msgs", 10000, "The maximum number of action messages to bundle togeter during ingestion")
	serveCmd.Flags().Int("number-of-run-msgs-transformers", 9, "The number of run messages to transform at a time")
	serveCmd.Flags().String("key", "key.pem", "SSL Private key for gRPC server")
	serveCmd.Flags().String("cert", "cert.pem", "SSL Certificate for gRPC server")
	serveCmd.Flags().String("root-cert", "cacert.pem", "Root SSL CA Certificate for gRPC server")
	viper.BindPFlags(serveCmd.Flags())
}
