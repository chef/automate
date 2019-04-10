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

var upgradeFrom1Long = `Upgrade an existing Chef Automate v1 deployment to Chef Automate v2.
	- <CONFIG_FILE> must be a valid path to a TOML formatted configuration file`

type upgradeCmdFlagSet struct {
	upgradeSkipPreflight      bool
	deliveryRunningPath       string
	deliverySecretsPath       string
	chefServerRunningPath     string
	upgradeTomlPath           string
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
	// airgapPreflight only applies to the preflight-check upgrade-from-v1
	// subcommand; we want to reuse all the skip*Check options there but for
	// preflight-check airgap is on/off vs. path to airgap bundle for the actual
	// upgrade.
	airgapPreflight  bool
	enableChefServer bool
	enableWorkflow   bool
}

var upgradeCmdFlags = upgradeCmdFlagSet{}

var upgradeFrom1Cmd = &cobra.Command{
	Use:   "upgrade-from-v1 [/path/to/automate-deploy.toml]",
	Short: "Upgrade from Chef Automate v1",
	Long:  upgradeFrom1Long,
	Args:  cobra.MaximumNArgs(3),
	RunE:  runUpgradeFromV1Cmd,
}

var upgradeFrom1StatusCmd = &cobra.Command{
	Use:   "upgrade-from-v1-status",
	Short: "Watch the status of the migration to Chef Automate 2",
	RunE:  runUpgradeFromV1StatusCmd,
}

var generateCfgCmd = &cobra.Command{
	Use:   "gen-config",
	Short: "Generate a config file",
	Long:  "Generate a Chef Automate v2 configuration file from Chef Automate v1",
	RunE:  runGenerateCfgCmd,
}

func init() {
	// upgrade-from-v1 flags
	upgradeFrom1Cmd.PersistentFlags().BoolVar(
		&upgradeCmdFlags.upgradeSkipPreflight,
		"skip-preflight",
		false,
		"Deploy regardless of pre-flight conditions")
	upgradeFrom1Cmd.PersistentFlags().StringVarP(
		&upgradeCmdFlags.deliverySecretsPath,
		"delivery-secrets",
		"s",
		"/etc/delivery/delivery-secrets.json",
		"Path to delivery-secrets.json")
	upgradeFrom1Cmd.PersistentFlags().StringVarP(
		&upgradeCmdFlags.deliveryRunningPath,
		"delivery-running",
		"r",
		"/etc/delivery/delivery-running.json",
		"Path to delivery-running.json")
	upgradeFrom1Cmd.PersistentFlags().StringVar(
		&upgradeCmdFlags.chefServerRunningPath,
		"chef-server-running",
		"/etc/opscode/chef-server-running.json",
		"Path to chef-server-running.json")
	upgradeFrom1Cmd.PersistentFlags().StringVarP(
		&upgradeCmdFlags.a2ConfigPath,
		"config",
		"c",
		"",
		"Path to an automate-deploy.toml")
	upgradeFrom1Cmd.PersistentFlags().StringVar(
		&upgradeCmdFlags.hartifactsPath,
		"hartifacts",
		"",
		"Optional path to cache of local .hart packages")
	upgradeFrom1Cmd.PersistentFlags().StringVar(
		&upgradeCmdFlags.overrideOrigin,
		"override-origin",
		"",
		"Optional origin to install local .hart packages from")
	upgradeFrom1Cmd.PersistentFlags().StringVar(
		&upgradeCmdFlags.manifestDir,
		"manifest-dir",
		"",
		"Directory of manifest files")
	upgradeFrom1Cmd.PersistentFlags().StringVar(
		&upgradeCmdFlags.channel,
		"channel",
		"",
		"Optional channel to use when installing packages from the depot")
	upgradeFrom1Cmd.PersistentFlags().StringVar(
		&upgradeCmdFlags.upgradeStrategy,
		"upgrade-strategy",
		"",
		"Optional upgrade strategy to use when configuring the deployment service")
	upgradeFrom1Cmd.PersistentFlags().IntVar(
		&upgradeCmdFlags.pgDumpSeconds,
		"postgres-dump-wait-seconds",
		0,
		"Optional timeout for Chef Automate v1 PostgreSQL dump (0 to disable timeout)")
	upgradeFrom1Cmd.PersistentFlags().IntVar(
		&upgradeCmdFlags.pgRestoreSeconds,
		"postgres-restore-wait-seconds",
		0,
		"Optional timeout for Chef Automate v1 PostgreSQL restore (0 to disable timeout)")
	upgradeFrom1Cmd.PersistentFlags().IntVar(
		&upgradeCmdFlags.fileMoveTimeout,
		"file-move-timeout",
		0,
		"Optional timeout for moving elasticsearch, compliance, and notifications files during Chef Automate v1 upgrade (0 to disable timeout)")
	upgradeFrom1Cmd.PersistentFlags().BoolVarP(
		&upgradeCmdFlags.yes,
		"yes",
		"y",
		false,
		"Do not prompt for confirmation; accept defaults and continue")
	upgradeFrom1Cmd.PersistentFlags().BoolVar(
		&upgradeCmdFlags.skipBackup,
		"skip-backup",
		false,
		"Optionally skip backup of your Chef Automate v1 installation (default = false)")
	upgradeFrom1Cmd.PersistentFlags().StringVar(
		&upgradeCmdFlags.adminPassword,
		"admin-password",
		"",
		"The password for the initial admin user. Auto-generated by default.")

	upgradeFrom1Cmd.PersistentFlags().BoolVar(
		&upgradeCmdFlags.selfTestMode,
		"self-test",
		false,
		"(DEV ONLY) execute upgrade against a test harness")

	upgradeFrom1Cmd.PersistentFlags().BoolVar(
		&upgradeCmdFlags.enableChefServer,
		"enable-chef-server",
		false,
		"Enable integrated Chef Server migration and deployment; only valid for all-in-one topology")

	upgradeFrom1Cmd.PersistentFlags().BoolVar(
		&upgradeCmdFlags.enableWorkflow,
		"enable-workflow",
		false,
		"Enable integrated Workflow migration and deployment; only valid for all-in-one topology")

	// passwords are not validated until the end of the upgrade, which makes this
	// feature dangerous. But we still want to have it in Ci, so we mark it as
	// hidden
	err := upgradeFrom1Cmd.PersistentFlags().MarkHidden("admin-password")
	if err != nil {
		fmt.Printf("failed configuring cobra: %s\n", err.Error())
		panic(":(")
	}
	// end users don't have any use for self-test, so don't show them
	err = upgradeFrom1Cmd.PersistentFlags().MarkHidden("self-test")
	if err != nil {
		fmt.Printf("failed configuring cobra: %s\n", err.Error())
		panic(":(")
	}

	// a1 upgrade with Chef Server is only supported for the all-in-one topology
	// used in OWCA and marketplace images; we do not support that configuration
	// for other customers, so it's hidden.
	err = upgradeFrom1Cmd.PersistentFlags().MarkHidden("enable-chef-server")
	if err != nil {
		fmt.Printf("failed configuring cobra: %s\n", err.Error())
		panic(":(")
	}

	// a1 upgrade with Workflow Server will be hidden until it is fully completed
	err = upgradeFrom1Cmd.PersistentFlags().MarkHidden("enable-workflow")
	if err != nil {
		fmt.Printf("failed configuring cobra: %s\n", err.Error())
		panic(":(")
	}

	// Also hide this because it's related to the chef-server feature
	err = upgradeFrom1Cmd.PersistentFlags().MarkHidden("chef-server-running")
	if err != nil {
		fmt.Printf("failed configuring cobra: %s\n", err.Error())
		panic(":(")
	}

	// upgrade-from-v1 gen-config flags
	generateCfgCmd.PersistentFlags().StringVarP(
		&upgradeCmdFlags.upgradeTomlPath,
		"out",
		"o",
		"./automate-upgrade.toml",
		"Output file")
	upgradeFrom1Cmd.PersistentFlags().BoolVar(
		&upgradeCmdFlags.skipBackupCheck,
		"skip-backup-check",
		false,
		"Optionally do not check if your Chef Automate v1 installation has backups configured (default = false)")
	upgradeFrom1Cmd.PersistentFlags().BoolVar(
		&upgradeCmdFlags.skipDisasterRecoveryCheck,
		"skip-disaster-recovery-check",
		false,
		"Optionally do not check if your Chef Automate v1 installation has disaster recovery configured (default = false)")
	upgradeFrom1Cmd.PersistentFlags().BoolVar(
		&upgradeCmdFlags.skipExternalESCheck,
		"skip-external-es-check",
		false,
		"Optionally do not check if your Chef Automate v1 installation has external Elasticsearch configured (default = false)")
	upgradeFrom1Cmd.PersistentFlags().BoolVar(
		&upgradeCmdFlags.skipFIPSCheck,
		"skip-fips-check",
		false,
		"Optionally do not check if your Chef Automate v1 installation has FIPS configured (default = false)")
	upgradeFrom1Cmd.PersistentFlags().BoolVar(
		&upgradeCmdFlags.skipSAMLCheck,
		"skip-saml-check",
		false,
		"Optionally do not check if your Chef Automate v1 installation has SAML configured (default = false)")
	upgradeFrom1Cmd.PersistentFlags().BoolVar(
		&upgradeCmdFlags.skipWorkflowCheck,
		"skip-workflow-check",
		false,
		"Optionally do not check if your Chef Automate v1 installation has workflow configured (default = false)")

	upgradeFrom1Cmd.PersistentFlags().StringVar(
		&upgradeCmdFlags.airgap,
		"airgap-bundle",
		"",
		"Path to an airgap install bundle")

	if !isDevMode() {
		for _, flagName := range []string{
			"override-origin",
			"hartifacts",
			"manifest-dir",
		} {
			err := upgradeFrom1Cmd.PersistentFlags().MarkHidden(flagName)
			if err != nil {
				fmt.Printf("failed configuring cobra: %s\n", err.Error())
				panic(":(")
			}
		}
	}

	upgradeFrom1Cmd.AddCommand(generateCfgCmd)
	RootCmd.AddCommand(upgradeFrom1Cmd)
	RootCmd.AddCommand(upgradeFrom1StatusCmd)
}

func runUpgradeFromV1Cmd(cmd *cobra.Command, args []string) error {
	cleanup := func() {
		if upgradeCmdFlags.selfTestMode {
			a1stub.CleanupTestHarness()
		}
	}
	defer cleanup()

	if upgradeCmdFlags.selfTestMode {
		err := a1stub.StartTestHarness()
		if err != nil {
			return status.Wrap(
				err,
				status.UpgradeError,
				"Starting the self-test harness failed",
			)
		}
	}

	// Initialize a new upgrade:
	//   * Load the given delivery-running.json and delivery-secrets.json files
	//   * Generate an A2 Config if an A2 configuration file was not passed.
	upgrade, err := newLocalUpgrade()
	if err != nil {
		return err
	}

	offlineMode := upgradeCmdFlags.airgap != ""
	manifestPath := ""
	if offlineMode {
		writer.Title("Installing airgap artifact")
		metadata, err := airgap.Unpack(upgradeCmdFlags.airgap)
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
		manifestPath = upgradeCmdFlags.manifestDir
	}

	manifestProvider := manifest.NewLocalHartManifestProvider(
		mc.NewDefaultClient(manifestPath),
		upgradeCmdFlags.hartifactsPath,
		upgradeCmdFlags.overrideOrigin)

	err = client.A1Upgrade(writer, upgrade, upgradeCmdFlags.yes, manifestProvider, version.BuildTime, offlineMode)
	if err != nil && !status.IsStatusError(err) {
		return status.Annotate(err, status.UpgradeError)
	}

	return err
}

func runUpgradeFromV1StatusCmd(cmd *cobra.Command, args []string) error {
	conn, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return err
	}

	stream, err := conn.A1UpgradeStatus(context.Background(), &api.A1UpgradeStatusRequest{})
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Acquiring upgrade status failed",
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
				"Reading message from the upgrade status stream failed",
			)
		}

		done, err := handler.HandleStatus(statusMsg)
		if err != nil {
			// The StatusHandler is responsible for printing the error
			return status.Wrap(
				err,
				status.UpgradeError,
				"Streaming upgrade status failed",
			)
		}

		if done {
			break
		}
	}

	return nil
}

func runGenerateCfgCmd(cmd *cobra.Command, args []string) error {
	upgrade, err := newLocalUpgrade()
	if err != nil {
		return err
	}

	if err = upgrade.A2Config.MarshalToTOMLFile(upgradeCmdFlags.upgradeTomlPath, 0600); err != nil {
		return status.Wrap(
			err,
			status.MarshalError,
			"Marshaling configuration to TOML file failed",
		)
	}

	return nil
}

func newLocalUpgrade() (*a1upgrade.A1Upgrade, error) {
	u, err := a1upgrade.NewA1Upgrade(
		a1upgrade.WithDeliveryRunning(upgradeCmdFlags.deliveryRunningPath),

		a1upgrade.WithDeliverySecrets(upgradeCmdFlags.deliverySecretsPath),

		a1upgrade.WithChefServerRunning(upgradeCmdFlags.chefServerRunningPath, upgradeCmdFlags.enableChefServer),

		a1upgrade.WithA2ConfigPath(upgradeCmdFlags.a2ConfigPath,
			dc.WithHartifacts(upgradeCmdFlags.hartifactsPath),
			dc.WithOrigin(upgradeCmdFlags.overrideOrigin)),

		a1upgrade.WithHartifactsPath(upgradeCmdFlags.hartifactsPath),

		a1upgrade.WithOverrideOrigin(upgradeCmdFlags.overrideOrigin),

		a1upgrade.WithManifestDir(upgradeCmdFlags.manifestDir),

		a1upgrade.WithChannel(upgradeCmdFlags.channel),

		a1upgrade.WithUpgradeStrategy(upgradeCmdFlags.upgradeStrategy),

		a1upgrade.WithAdminPassword(upgradeCmdFlags.adminPassword),

		a1upgrade.SkipUpgradePreflight(upgradeCmdFlags.upgradeSkipPreflight),

		a1upgrade.SetPostgresDumpWait(upgradeCmdFlags.pgDumpSeconds),

		a1upgrade.SetPostgresRestoreWait(upgradeCmdFlags.pgRestoreSeconds),

		a1upgrade.SetFileMoveTimeout(upgradeCmdFlags.fileMoveTimeout),

		a1upgrade.SkipUpgradeBackup(upgradeCmdFlags.skipBackup),

		a1upgrade.SkipBackupConfiguredCheck(upgradeCmdFlags.skipBackupCheck),

		a1upgrade.SkipDisasterRecoveryConfiguredCheck(upgradeCmdFlags.skipDisasterRecoveryCheck),

		a1upgrade.SkipExternalESConfiguredCheck(upgradeCmdFlags.skipExternalESCheck),

		a1upgrade.SkipFIPSConfiguredCheck(upgradeCmdFlags.skipFIPSCheck),

		a1upgrade.SkipSAMLConfiguredCheck(upgradeCmdFlags.skipSAMLCheck),

		a1upgrade.SkipWorkflowConfiguredCheck(upgradeCmdFlags.skipWorkflowCheck),

		a1upgrade.WithChefServerEnabled(upgradeCmdFlags.enableChefServer),

		a1upgrade.WithWorkflowEnabled(upgradeCmdFlags.enableWorkflow),
	)

	if err != nil {
		return nil, status.Wrap(
			err,
			status.UpgradeError,
			"Creating A1 migrator failed",
		)
	}

	if err := u.GenerateA2ConfigIfNoneProvided(upgradeCmdFlags.a2ConfigPath); err != nil {
		return nil, status.Wrap(err, status.ConfigError, "Generating Chef Automate configuration failed")
	}

	return u, nil
}
