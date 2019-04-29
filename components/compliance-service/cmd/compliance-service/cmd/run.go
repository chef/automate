package cmd

import (
	"fmt"

	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"os"

	compliance "github.com/chef/automate/components/compliance-service"
	"github.com/chef/automate/components/compliance-service/config"
	"github.com/chef/automate/lib/platform"
	"github.com/chef/automate/lib/tracing"
)

// RootCmd represents the base command when called without any subcommands
var RootCmd = &cobra.Command{
	Use:   config.SERVICE_NAME,
	Short: "Compliance service",
	Long:  `Please use the run command to start the service with the appropriate flags.`,
}

var conf = config.Compliance{
	Postgres: config.Postgres{
		ConnectionString: "postgresql://127.0.0.1:5432/chef_automate",
		MigrationsPath:   "dao/pgdb/migration/sql/",
	},
	InspecAgent: config.InspecAgent{
		JobBufferSize: 1000,
		JobWorkers:    10,
		BackendCache:  "true",
		AuthnTarget:   "0.0.0.0:10113",
		AutomateFQDN:  "localhost",
		TmpDir:        os.Getenv("TMPDIR"),
	},
	ElasticSearch: config.ElasticSearch{},
	Profiles: config.Profiles{
		MarketPath:   "./market",
		ProfilesPath: "./profiles",
	},
	Service: config.Service{
		Name:     "compliance",
		Port:     10121,
		HostBind: "127.0.0.1",
	},
	Secrets:       config.Secrets{},
	Notifications: config.Notifications{},
	Manager: config.Manager{
		ManualPollIntervalMinutes:  1440,
		AwsEc2PollIntervalMinutes:  60,
		AzureVMPollIntervalMinutes: 180,
	},
	EventConfig: config.EventConfig{},
	FeedConfig:  config.FeedConfig{},
	DataRetention: config.DataRetention{
		ComplianceReportDays: -1,
		ComplianceEventDays:  7,
	},
}

// runCmd represents the run command
var runCmd = &cobra.Command{
	Use:   "run",
	Short: "Run the compliance service.",
	Long:  `The compliance service includes reporting routes, scanner routes, and the inspec agent.`,
	Run: func(cmd *cobra.Command, args []string) {
		closer, err := tracing.NewGlobalTracer(config.SERVICE_NAME)
		if err == nil {
			defer tracing.CloseQuietly(closer)
		}

		if conf.Postgres.Database != "" {
			conf.Postgres.ConnectionString, err = platform.PGURIFromEnvironment(conf.Postgres.Database)
			if err != nil {
				logrus.WithError(err).Fatal("Failed to get pg uri from platform config")
			}
		}

		grpcBinding := fmt.Sprintf("%s:%d", conf.Service.HostBind, conf.Service.Port)
		err = compliance.Serve(conf, grpcBinding)
		if err == nil {
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
	runCmd.Flags().Int32Var(&conf.DataRetention.ComplianceReportDays, "reports-retention-days", -1, "Number of days to keep compliance reports")
	runCmd.Flags().StringVar(&conf.Service.ConfigFilePath, "config", "", "config file")

	// Postgres Config Flags
	runCmd.Flags().StringVar(&conf.Postgres.ConnectionString, "postgres-uri", conf.Postgres.ConnectionString, "PostgreSQL connection string to use")
	runCmd.Flags().StringVar(&conf.Postgres.Database, "postgres-database", "", "PostgreSQL database to use. Will override postgres-uri")
	runCmd.Flags().StringVar(&conf.Postgres.MigrationsPath, "migrations-path", conf.Postgres.MigrationsPath, "Path to sql files for migration")

	// ElasticSearch Config Flags
	runCmd.Flags().StringVar(&conf.ElasticSearch.Url, "es-url", conf.ElasticSearch.Url, "ES Url")
	runCmd.Flags().StringVar(&conf.ESSidecarAddress, "es-sidecar-address", conf.ESSidecarAddress, "ES Sidecar Address")

	// Profiles Config Flags
	runCmd.Flags().StringVar(&conf.Profiles.MarketPath, "market-path", conf.Profiles.MarketPath, "Market Profiles Path")
	runCmd.Flags().StringVar(&conf.Profiles.ProfilesPath, "profiles-path", conf.Profiles.ProfilesPath, "Profiles Path")

	// InspecAgent Config Flags
	runCmd.Flags().IntVar(&conf.InspecAgent.JobWorkers, "job-workers", conf.InspecAgent.JobWorkers, "Max number of inspec workers to run in parallel for detect and scan jobs")
	runCmd.Flags().IntVar(&conf.InspecAgent.JobBufferSize, "job-buffer-size", conf.InspecAgent.JobBufferSize, "Max number of detect and scan jobs that can be accepted in the jobs workers queue")
	runCmd.Flags().StringVar(&conf.InspecAgent.BackendCache, "backend-cache", conf.InspecAgent.BackendCache, "Option to use the backend cache functionality of InSpec, 'true' or 'false'")
	runCmd.Flags().StringVar(&conf.InspecAgent.AuthnTarget, "authn-target", conf.InspecAgent.AuthnTarget, "Target grpc dial address for authn service, used to create token for reporting to automate")
	runCmd.Flags().StringVar(&conf.InspecAgent.AutomateFQDN, "automate-fqdn", conf.InspecAgent.AutomateFQDN, "Target fqdn for inspec reporting to automate")
	runCmd.Flags().StringVar(&conf.InspecAgent.TmpDir, "inspec-tmp-dir", conf.InspecAgent.TmpDir, "location of /tmp dir to be used by inspec for caching")

	// Legacy Automate Headers/User Info
	runCmd.Flags().StringVar(&conf.Delivery.Enterprise, "delivery-ent", conf.Delivery.Enterprise, "Automate Enterprise")
	runCmd.Flags().StringVar(&conf.Delivery.User, "chef-delivery-user", conf.Delivery.User, "Automate User")
	runCmd.Flags().StringVar(&conf.Delivery.Token, "chef-delivery-token", conf.Delivery.Token, "Automate Token")
	runCmd.Flags().StringVar(&conf.Delivery.DCToken, "chef-delivery-dctoken", conf.Delivery.DCToken, "The delivery data collector token needed to post scan reports to compliance. Can also be provided via the DC_TOKEN env variable")

	// Notifications Flag
	runCmd.Flags().StringVar(&conf.Notifications.Target, "notifications-target", conf.Notifications.Target, "Target grpc dial address for notifications service, used to send notifications on report ingestion")

	// Node Manager Lifecycle Polling Config Flags
	runCmd.Flags().IntVar(&conf.Manager.Port, "manager-port", conf.Manager.Port, "Nodemanager Port")
	runCmd.Flags().StringVar(&conf.Manager.Host, "manager-host", conf.Manager.Host, "Nodemanager Host")
	runCmd.Flags().IntVar(&conf.Manager.ManualPollIntervalMinutes, "manager-manual-poll", conf.Manager.ManualPollIntervalMinutes, "The interval (an integer in minutes) at which the user would like to poll manually added nodes' status")
	runCmd.Flags().IntVar(&conf.Manager.AwsEc2PollIntervalMinutes, "manager-awsec2-poll", conf.Manager.AwsEc2PollIntervalMinutes, "The interval (an integer in minutes) at which the user would like to poll for awsec2 instance state updates")
	runCmd.Flags().IntVar(&conf.Manager.AzureVMPollIntervalMinutes, "manager-azurevm-poll", conf.Manager.AzureVMPollIntervalMinutes, "The interval (an integer in minutes) at which the user would like to poll for azure vm instance state updates")

	// Secrets Service Flags
	runCmd.Flags().StringVar(&conf.Secrets.HostBind, "secrets-host", conf.Secrets.HostBind, "Secrets Service Host")
	runCmd.Flags().IntVar(&conf.Secrets.Port, "secrets-port", conf.Secrets.Port, "Secrets Service Port")

	// Authz Service Flags
	runCmd.Flags().StringVar(&conf.Authz.HostBind, "authz-host", conf.Authz.HostBind, "Authz Service Host")
	runCmd.Flags().IntVar(&conf.Authz.Port, "authz-port", conf.Authz.Port, "Authz Service Port")

	// Event Service Flags
	runCmd.Flags().StringVar(&conf.EventConfig.Endpoint, "event-endpoint", conf.EventConfig.Endpoint, "Event Service Endpoint")
}
