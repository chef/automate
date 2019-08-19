package cmd

import (
	"fmt"

	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	nodemanager "github.com/chef/automate/components/nodemanager-service"
	"github.com/chef/automate/components/nodemanager-service/config"
	platform_config "github.com/chef/automate/lib/platform/config"
	"github.com/chef/automate/lib/tracing"
)

// RootCmd represents the base command when called without any subcommands
var RootCmd = &cobra.Command{
	Use:   config.SERVICE_NAME,
	Short: "Nodemanager service",
	Long:  `Please use the run command to start the service with the appropriate flags.`,
}

var conf = config.Nodemanager{
	Postgres: config.Postgres{
		ConnectionString: "postgresql://127.0.0.1:5432/chef_automate",
		MigrationsPath:   "pgdb/migration/sql/",
	},
	Service: config.Service{
		Name:     "nodemanager-service",
		Port:     10120,
		HostBind: "127.0.0.1",
	},
	Secrets: config.Secrets{
		Port:     10131,
		HostBind: "127.0.0.1",
	},
	Manager: config.Manager{
		ManualPollIntervalMinutes:  1440,
		AwsEc2PollIntervalMinutes:  60,
		AzureVMPollIntervalMinutes: 180,
	},
	EventConfig: config.EventConfig{
		Endpoint: "127.0.0.1:10132",
	},
}

// runCmd represents the run command
var runCmd = &cobra.Command{
	Use:   "run",
	Short: "Run the nodemanager service.",
	Long:  `The nodemanager service includes nodes and nodemanagers logic.`,
	Run: func(cmd *cobra.Command, args []string) {
		closer, err := tracing.NewGlobalTracer(config.SERVICE_NAME)
		if err == nil {
			defer tracing.CloseQuietly(closer)
		}
		if conf.Postgres.Database != "" {
			conf.Postgres.ConnectionString, err = platform_config.PGURIFromEnvironment(conf.Postgres.Database)
			if err != nil {
				logrus.WithError(err).Fatal("Failed to get pg uri")
			}
		}
		grpcBinding := fmt.Sprintf("%s:%d", conf.Service.HostBind, conf.Service.Port)
		logrus.Infof("starting nodemanager service on: %s", grpcBinding)
		err = nodemanager.Serve(conf, grpcBinding)
		if err != nil {
			logrus.WithError(err).Fatal("Unable to serve the backend")
		}
	},
}

func init() {
	RootCmd.AddCommand(runCmd)

	// General Service Flags
	runCmd.Flags().StringVar(&conf.Service.HostBind, "host", conf.Service.HostBind, "Host")
	runCmd.Flags().IntVar(&conf.Service.Port, "port", conf.Service.Port, "Port")
	runCmd.Flags().StringVar(&conf.Service.LogLevel, "log-level", conf.Service.LogLevel, "Log Level")
	runCmd.Flags().StringVar(&conf.Service.TLSConfig.CertPath, "cert", "", "Service certificate")
	runCmd.Flags().StringVar(&conf.Service.TLSConfig.KeyPath, "key", "", "Service certificate key")
	runCmd.Flags().StringVar(&conf.Service.TLSConfig.RootCACertPath, "root-cert", "", "Root CA Cert to use to verify clients")

	// Postgres Config Flags
	runCmd.Flags().StringVar(&conf.Postgres.ConnectionString, "postgres-uri", conf.Postgres.ConnectionString, "PostgreSQL connection string to use")
	runCmd.Flags().StringVar(&conf.Postgres.Database, "postgres-database", "", "PostgreSQL database to use. Will override postgres-uri")
	runCmd.Flags().StringVar(&conf.Postgres.MigrationsPath, "migrations-path", conf.Postgres.MigrationsPath, "Path to sql files for migration")

	// Node Manager Lifecycle Polling Config Flags
	runCmd.Flags().IntVar(&conf.Manager.ManualPollIntervalMinutes, "manager-manual-poll", conf.Manager.ManualPollIntervalMinutes, "The interval (an integer in minutes) at which the user would like to poll manually added nodes' status")
	runCmd.Flags().IntVar(&conf.Manager.AwsEc2PollIntervalMinutes, "manager-awsec2-poll", conf.Manager.AwsEc2PollIntervalMinutes, "The interval (an integer in minutes) at which the user would like to poll for awsec2 instance state updates")
	runCmd.Flags().IntVar(&conf.Manager.AzureVMPollIntervalMinutes, "manager-azurevm-poll", conf.Manager.AzureVMPollIntervalMinutes, "The interval (an integer in minutes) at which the user would like to poll for azure vm instance state updates")

	// Secrets Service Flags
	runCmd.Flags().StringVar(&conf.Secrets.HostBind, "secrets-host", conf.Secrets.HostBind, "Secrets Service Host")
	runCmd.Flags().IntVar(&conf.Secrets.Port, "secrets-port", conf.Secrets.Port, "Secrets Service Port")

	// Event Service Flags
	runCmd.Flags().StringVar(&conf.EventConfig.Endpoint, "event-endpoint", conf.EventConfig.Endpoint, "Event Service Endpoint")

}
