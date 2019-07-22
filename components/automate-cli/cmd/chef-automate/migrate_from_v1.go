// Copyright © 2017 Chef Software

package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"path"

	"github.com/spf13/cobra"
	"golang.org/x/crypto/ssh/terminal"

	dc "github.com/chef/automate/api/config/deployment"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/a1stub"
	"github.com/chef/automate/components/automate-deployment/pkg/a1upgrade"
	"github.com/chef/automate/components/automate-deployment/pkg/airgap"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	mc "github.com/chef/automate/components/automate-deployment/pkg/manifest/client"
	"github.com/chef/automate/lib/version"
)

var migrateFrom1Long = `Migrate an existing Chef Automate v1 deployment to Chef Automate v2.
	- <CONFIG_FILE> must be a valid path to a TOML formatted configuration file`

type migrateCmdFlagSet struct {
	migrateSkipPreflight      bool
	deliveryRunningPath       string
	deliverySecretsPath       string
	chefServerRunningPath     string
	migrateTomlPath           string
	a2ConfigPath              string
	hartifactsPath            string
	manifestDir               string
	channel                   string
	overrideOrigin            string
	upgradeStrategy           string
	adminPassword             string
	selfTestMode              bool
	pgDumpSeconds             int
	pgRestoreSeconds          int
	yes                       bool
	skipBackup                bool
	fileMoveTimeout           int
	skipBackupCheck           bool
	skipDisasterRecoveryCheck bool
	skipExternalESCheck       bool
	skipFIPSCheck             bool
	skipSAMLCheck             bool
	skipWorkflowCheck         bool
	airgap                    string
	// airgapPreflight only applies to the preflight-check migrate-from-v1
	// subcommand; we want to reuse all the skip*Check options there but for
	// preflight-check airgap is on/off vs. path to airgap bundle for the actual
	// migration.
	airgapPreflight  bool
	enableChefServer bool
	enableWorkflow   bool
}

var migrateCmdFlags = migrateCmdFlagSet{}

var migrateFrom1Cmd = &cobra.Command{
	Use:     "migrate-from-v1 [/path/to/automate-deploy.toml]",
	Short:   "Migrate from Chef Automate v1",
	Long:    migrateFrom1Long,
	Args:    cobra.MaximumNArgs(3),
	RunE:    runMigrateFromV1Cmd,
	Aliases: []string{"upgrade-from-v1"},
}

var migrateFrom1StatusCmd = &cobra.Command{
	Use:   "migrate-from-v1-status",
	Short: "Watch the status of the migration to Chef Automate 2",
	RunE:  runMigrationFromV1StatusCmd,
}

var generateCfgCmd = &cobra.Command{
	Use:   "gen-config",
	Short: "Generate a config file",
	Long:  "Generate a Chef Automate v2 configuration file from Chef Automate v1",
	RunE:  runGenerateCfgCmd,
}

func init() {
	// migrate-from-v1 flags
	migrateFrom1Cmd.PersistentFlags().BoolVar(
		&migrateCmdFlags.migrateSkipPreflight,
		"skip-preflight",
		false,
		"Deploy regardless of pre-flight conditions")
	migrateFrom1Cmd.PersistentFlags().StringVarP(
		&migrateCmdFlags.deliverySecretsPath,
		"delivery-secrets",
		"s",
		"/etc/delivery/delivery-secrets.json",
		"Path to delivery-secrets.json")
	migrateFrom1Cmd.PersistentFlags().StringVarP(
		&migrateCmdFlags.deliveryRunningPath,
		"delivery-running",
		"r",
		"/etc/delivery/delivery-running.json",
		"Path to delivery-running.json")
	migrateFrom1Cmd.PersistentFlags().StringVar(
		&migrateCmdFlags.chefServerRunningPath,
		"chef-server-running",
		"/etc/opscode/chef-server-running.json",
		"Path to chef-server-running.json")
	migrateFrom1Cmd.PersistentFlags().StringVarP(
		&migrateCmdFlags.a2ConfigPath,
		"config",
		"c",
		"",
		"Path to an automate-deploy.toml")
	migrateFrom1Cmd.PersistentFlags().StringVar(
		&migrateCmdFlags.hartifactsPath,
		"hartifacts",
		"",
		"Optional path to cache of local .hart packages")
	migrateFrom1Cmd.PersistentFlags().StringVar(
		&migrateCmdFlags.overrideOrigin,
		"override-origin",
		"",
		"Optional origin to install local .hart packages from")
	migrateFrom1Cmd.PersistentFlags().StringVar(
		&migrateCmdFlags.manifestDir,
		"manifest-dir",
		"",
		"Directory of manifest files")
	migrateFrom1Cmd.PersistentFlags().StringVar(
		&migrateCmdFlags.channel,
		"channel",
		"",
		"Optional channel to use when installing packages from the depot")
	migrateFrom1Cmd.PersistentFlags().StringVar(
		&migrateCmdFlags.upgradeStrategy,
		"upgrade-strategy",
		"",
		"Optional upgrade strategy to use when configuring the deployment service")
	migrateFrom1Cmd.PersistentFlags().IntVar(
		&migrateCmdFlags.pgDumpSeconds,
		"postgres-dump-wait-seconds",
		0,
		"Optional timeout for Chef Automate v1 PostgreSQL dump (0 to disable timeout)")
	migrateFrom1Cmd.PersistentFlags().IntVar(
		&migrateCmdFlags.pgRestoreSeconds,
		"postgres-restore-wait-seconds",
		0,
		"Optional timeout for Chef Automate v1 PostgreSQL restore (0 to disable timeout)")
	migrateFrom1Cmd.PersistentFlags().IntVar(
		&migrateCmdFlags.fileMoveTimeout,
		"file-move-timeout",
		0,
		"Optional timeout for moving elasticsearch, compliance, and notifications files during Chef Automate v1 migration (0 to disable timeout)")
	migrateFrom1Cmd.PersistentFlags().BoolVarP(
		&migrateCmdFlags.yes,
		"yes",
		"y",
		false,
		"Do not prompt for confirmation; accept defaults and continue")
	migrateFrom1Cmd.PersistentFlags().BoolVar(
		&migrateCmdFlags.skipBackup,
		"skip-backup",
		false,
		"Optionally skip backup of your Chef Automate v1 installation (default = false)")
	migrateFrom1Cmd.PersistentFlags().StringVar(
		&migrateCmdFlags.adminPassword,
		"admin-password",
		"",
		"The password for the initial admin user. Auto-generated by default.")

	migrateFrom1Cmd.PersistentFlags().BoolVar(
		&migrateCmdFlags.selfTestMode,
		"self-test",
		false,
		"(DEV ONLY) execute migration against a test harness")

	migrateFrom1Cmd.PersistentFlags().BoolVar(
		&migrateCmdFlags.enableChefServer,
		"enable-chef-server",
		false,
		"Enable integrated Chef Server migration and deployment; only valid for all-in-one topology")

	migrateFrom1Cmd.PersistentFlags().BoolVar(
		&migrateCmdFlags.enableWorkflow,
		"enable-workflow",
		false,
		"Enable integrated Workflow migration and deployment; only valid for all-in-one topology")

	// passwords are not validated until the end of the migration, which makes this
	// feature dangerous. But we still want to have it in Ci, so we mark it as
	// hidden
	err := migrateFrom1Cmd.PersistentFlags().MarkHidden("admin-password")
	if err != nil {
		fmt.Printf("failed configuring cobra: %s\n", err.Error())
		panic(":(")
	}
	// end users don't have any use for self-test, so don't show them
	err = migrateFrom1Cmd.PersistentFlags().MarkHidden("self-test")
	if err != nil {
		fmt.Printf("failed configuring cobra: %s\n", err.Error())
		panic(":(")
	}

	// a1 migration with Chef Server is only supported for the all-in-one topology
	// used in OWCA and marketplace images; we do not support that configuration
	// for other customers, so it's hidden.
	err = migrateFrom1Cmd.PersistentFlags().MarkHidden("enable-chef-server")
	if err != nil {
		fmt.Printf("failed configuring cobra: %s\n", err.Error())
		panic(":(")
	}

	// a1 migration with Workflow Server will be hidden until it is fully completed
	err = migrateFrom1Cmd.PersistentFlags().MarkHidden("enable-workflow")
	if err != nil {
		fmt.Printf("failed configuring cobra: %s\n", err.Error())
		panic(":(")
	}

	// Also hide this because it's related to the chef-server feature
	err = migrateFrom1Cmd.PersistentFlags().MarkHidden("chef-server-running")
	if err != nil {
		fmt.Printf("failed configuring cobra: %s\n", err.Error())
		panic(":(")
	}

	// migrate-from-v1 gen-config flags
	generateCfgCmd.PersistentFlags().StringVarP(
		&migrateCmdFlags.migrateTomlPath,
		"out",
		"o",
		"./automate-migrate.toml",
		"Output file")
	migrateFrom1Cmd.PersistentFlags().BoolVar(
		&migrateCmdFlags.skipBackupCheck,
		"skip-backup-check",
		false,
		"Optionally do not check if your Chef Automate v1 installation has backups configured (default = false)")
	migrateFrom1Cmd.PersistentFlags().BoolVar(
		&migrateCmdFlags.skipDisasterRecoveryCheck,
		"skip-disaster-recovery-check",
		false,
		"Optionally do not check if your Chef Automate v1 installation has disaster recovery configured (default = false)")
	migrateFrom1Cmd.PersistentFlags().BoolVar(
		&migrateCmdFlags.skipExternalESCheck,
		"skip-external-es-check",
		false,
		"Optionally do not check if your Chef Automate v1 installation has external Elasticsearch configured (default = false)")
	migrateFrom1Cmd.PersistentFlags().BoolVar(
		&migrateCmdFlags.skipFIPSCheck,
		"skip-fips-check",
		false,
		"Optionally do not check if your Chef Automate v1 installation has FIPS configured (default = false)")
	migrateFrom1Cmd.PersistentFlags().BoolVar(
		&migrateCmdFlags.skipSAMLCheck,
		"skip-saml-check",
		false,
		"Optionally do not check if your Chef Automate v1 installation has SAML configured (default = false)")
	migrateFrom1Cmd.PersistentFlags().BoolVar(
		&migrateCmdFlags.skipWorkflowCheck,
		"skip-workflow-check",
		false,
		"Optionally do not check if your Chef Automate v1 installation has workflow configured (default = false)")

	migrateFrom1Cmd.PersistentFlags().StringVar(
		&migrateCmdFlags.airgap,
		"airgap-bundle",
		"",
		"Path to an airgap install bundle")

	if !isDevMode() {
		for _, flagName := range []string{
			"override-origin",
			"hartifacts",
			"manifest-dir",
		} {
			err := migrateFrom1Cmd.PersistentFlags().MarkHidden(flagName)
			if err != nil {
				fmt.Printf("failed configuring cobra: %s\n", err.Error())
				panic(":(")
			}
		}
	}

	migrateFrom1Cmd.AddCommand(generateCfgCmd)
	RootCmd.AddCommand(migrateFrom1Cmd)
	RootCmd.AddCommand(migrateFrom1StatusCmd)
}

func runMigrateFromV1Cmd(cmd *cobra.Command, args []string) error {
	cleanup := func() {
		if migrateCmdFlags.selfTestMode {
			a1stub.CleanupTestHarness()
		}
	}
	defer cleanup()

	if migrateCmdFlags.selfTestMode {
		err := a1stub.StartTestHarness()
		if err != nil {
			return status.Wrap(
				err,
				status.UpgradeError,
				"Starting the self-test harness failed",
			)
		}
	}

	// Initialize a new migration:
	//   * Load the given delivery-running.json and delivery-secrets.json files
	//   * Generate an A2 Config if an A2 configuration file was not passed.
	migration, err := newLocalMigration()
	if err != nil {
		return err
	}

	offlineMode := migrateCmdFlags.airgap != ""
	manifestPath := ""
	if offlineMode {
		writer.Title("Installing airgap artifact")
		metadata, err := airgap.Unpack(migrateCmdFlags.airgap)
		if err != nil {
			return status.Annotate(err, status.AirgapUnpackInstallBundleError)
		}
		manifestPath = api.AirgapManifestPath
		pathEnv := os.Getenv("PATH")

		// We need to set the PATH here to include hab so that bootstrapping A2 uses that
		// hab instead of trying to download it from the internet
		err = os.Setenv("PATH", fmt.Sprintf("%s:%s", path.Dir(metadata.HabBinPath), pathEnv))
		if err != nil {
			return err
		}
	} else {
		manifestPath = migrateCmdFlags.manifestDir
	}

	manifestProvider := manifest.NewLocalHartManifestProvider(
		mc.NewDefaultClient(manifestPath),
		migrateCmdFlags.hartifactsPath,
		migrateCmdFlags.overrideOrigin)

	err = client.A1Upgrade(writer, migration, migrateCmdFlags.yes, manifestProvider, version.BuildTime, offlineMode)
	if err != nil && !status.IsStatusError(err) {
		return status.Annotate(err, status.UpgradeError)
	}

	return err
}

func runMigrationFromV1StatusCmd(cmd *cobra.Command, args []string) error {
	conn, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return err
	}

	stream, err := conn.A1UpgradeStatus(context.Background(), &api.A1UpgradeStatusRequest{})
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Acquiring migration status failed",
		)
	}

	opts := []a1upgrade.StatusHandlerOpt{
		a1upgrade.WithWriter(writer),
	}
	if !terminal.IsTerminal(1) {
		opts = append(opts, a1upgrade.NoTTY())
	}

	handler := a1upgrade.NewStatusHandler(opts...)
	for {
		statusMsg, err := stream.Recv()
		if err == io.EOF {
			break
		}

		if err != nil {
			return status.Wrap(
				err,
				status.DeploymentServiceCallError,
				"Reading message from the migration status stream failed",
			)
		}

		done, err := handler.HandleStatus(statusMsg)
		if err != nil {
			// The StatusHandler is responsible for printing the error
			return status.Wrap(
				err,
				status.UpgradeError,
				"Streaming migration status failed",
			)
		}

		if done {
			break
		}
	}

	return nil
}

func runGenerateCfgCmd(cmd *cobra.Command, args []string) error {
	migration, err := newLocalMigration()
	if err != nil {
		return err
	}

	if err = migration.A2Config.MarshalToTOMLFile(migrateCmdFlags.migrateTomlPath, 0600); err != nil {
		return status.Wrap(
			err,
			status.MarshalError,
			"Marshaling configuration to TOML file failed",
		)
	}

	return nil
}

func newLocalMigration() (*a1upgrade.A1Upgrade, error) {
	u, err := a1upgrade.NewA1Upgrade(
		a1upgrade.WithDeliveryRunning(migrateCmdFlags.deliveryRunningPath),

		a1upgrade.WithDeliverySecrets(migrateCmdFlags.deliverySecretsPath),

		a1upgrade.WithChefServerRunning(migrateCmdFlags.chefServerRunningPath, migrateCmdFlags.enableChefServer),

		a1upgrade.WithA2ConfigPath(migrateCmdFlags.a2ConfigPath,
			dc.WithHartifacts(migrateCmdFlags.hartifactsPath),
			dc.WithOrigin(migrateCmdFlags.overrideOrigin)),

		a1upgrade.WithHartifactsPath(migrateCmdFlags.hartifactsPath),

		a1upgrade.WithOverrideOrigin(migrateCmdFlags.overrideOrigin),

		a1upgrade.WithManifestDir(migrateCmdFlags.manifestDir),

		a1upgrade.WithChannel(migrateCmdFlags.channel),

		a1upgrade.WithUpgradeStrategy(migrateCmdFlags.upgradeStrategy),

		a1upgrade.WithAdminPassword(migrateCmdFlags.adminPassword),

		a1upgrade.SkipUpgradePreflight(migrateCmdFlags.migrateSkipPreflight),

		a1upgrade.SetPostgresDumpWait(migrateCmdFlags.pgDumpSeconds),

		a1upgrade.SetPostgresRestoreWait(migrateCmdFlags.pgRestoreSeconds),

		a1upgrade.SetFileMoveTimeout(migrateCmdFlags.fileMoveTimeout),

		a1upgrade.SkipUpgradeBackup(migrateCmdFlags.skipBackup),

		a1upgrade.SkipBackupConfiguredCheck(migrateCmdFlags.skipBackupCheck),

		a1upgrade.SkipDisasterRecoveryConfiguredCheck(migrateCmdFlags.skipDisasterRecoveryCheck),

		a1upgrade.SkipExternalESConfiguredCheck(migrateCmdFlags.skipExternalESCheck),

		a1upgrade.SkipFIPSConfiguredCheck(migrateCmdFlags.skipFIPSCheck),

		a1upgrade.SkipSAMLConfiguredCheck(migrateCmdFlags.skipSAMLCheck),

		a1upgrade.SkipWorkflowConfiguredCheck(migrateCmdFlags.skipWorkflowCheck),

		a1upgrade.WithChefServerEnabled(migrateCmdFlags.enableChefServer),

		a1upgrade.WithWorkflowEnabled(migrateCmdFlags.enableWorkflow),
	)

	if err != nil {
		return nil, status.Wrap(
			err,
			status.UpgradeError,
			"Creating A1 migrator failed",
		)
	}

	if err := u.GenerateA2ConfigIfNoneProvided(migrateCmdFlags.a2ConfigPath); err != nil {
		return nil, status.Wrap(err, status.ConfigError, "Generating Chef Automate configuration failed")
	}

	return u, nil
}
