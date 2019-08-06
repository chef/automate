package main

import (
	"context"

	"github.com/spf13/cobra"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/a1upgrade"
	"github.com/chef/automate/components/automate-deployment/pkg/airgap"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/lib/io/fileutils"
)

var upgradeCmd = &cobra.Command{
	Use:   "upgrade COMMAND",
	Short: "upgrade automate to the latest version",
}

var upgradeRunCmdFlags = struct {
	airgap  string
	version string
}{}

var upgradeRunCmd = &cobra.Command{
	Use:   "run",
	Short: "Run an upgrade of Chef Automate",
	Long:  "Run an upgrade of Chef Automate",
	RunE:  runUpgradeCmd,
	Args:  cobra.MaximumNArgs(0),
}

var upgradeStatusCmd = &cobra.Command{
	Use:   "status",
	Short: "Get upgrade status of Chef Automate",
	Long:  "Get upgrade status of Chef Automate",
	RunE:  statusUpgradeCmd,
	Args:  cobra.MaximumNArgs(0),
}

var a1RunningMsg = "You have a running Chef Automate v1 installation. Did you mean to type `chef-automate upgrade-from-v1` (alias for: `chef-automate migrate-from-v1`)?"

func runUpgradeCmd(cmd *cobra.Command, args []string) error {
	a1IsRunning, err := isA1Running()
	if err != nil {
		return status.Annotate(err, status.FileAccessError)
	}

	if a1IsRunning {
		yes, err := writer.Confirm(a1RunningMsg)

		if err != nil {
			return status.Annotate(err, status.UpgradeError)
		}

		if yes {
			return status.New(status.UpgradeError, "`chef-automate upgrade-from-v1` (alias for: `chef-automate migrate-from-v1`) intended.")
		}
	}

	offlineMode := upgradeRunCmdFlags.airgap != ""

	if airgap.AirgapInUse() && !offlineMode {
		return status.New(status.InvalidCommandArgsError, "To upgrade a deployment created with an airgap bundle, use --airgap-bundle to specify a bundle to use for the upgrade.")
	}

	if upgradeRunCmdFlags.version != "" && offlineMode {
		return status.New(status.InvalidCommandArgsError, "--version and --airgap-bundle cannot be used together")
	}

	if offlineMode {
		writer.Title("Installing airgap install bundle")
		_, err := airgap.Unpack(upgradeRunCmdFlags.airgap)
		if err != nil {
			return status.Annotate(err, status.AirgapUnpackInstallBundleError)
		}
	}

	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return err
	}
	resp, err := connection.Upgrade(context.Background(), &api.UpgradeRequest{
		Version: upgradeRunCmdFlags.version,
	})
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to start upgrade failed",
		)
	}
	if resp.NextVersion != resp.PreviousVersion {
		writer.Println("Upgrading Chef Automate")
	} else {
		//TODO(jaym): This is a bit of a lie. We don't factor hartifact overrides
		//            into this calculation
		writer.Println("Chef Automate up-to-date")
	}

	// TODO(jaym): stream back events
	// The reason for this is because our streaming
	// stuff breaks when deployment-service is restarted. Until that's fixed,
	// it would be pointless to try to stream the events.
	return nil
}

func statusUpgradeCmd(cmd *cobra.Command, args []string) error {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return err
	}

	resp, err := connection.UpgradeStatus(context.Background(), &api.UpgradeStatusRequest{})
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to get upgrade status failed",
		)
	}

	// TODO(ssd) 2018-09-17: This API response was built around
	// the world where we didn't /know/ that an upgrade was
	// happening or not. Now we should "just know" and the API
	// here is pretty strange. Why are we doing all of this logic
	// client side?
	//
	switch resp.State {
	case api.UpgradeStatusResponse_IDLE:
		switch {
		case resp.CurrentVersion != "" && resp.CurrentVersion < resp.LatestAvailableVersion:
			writer.Printf("Automate is out-of-date (current version: %s; latest available: %s)\n",
				resp.CurrentVersion, resp.LatestAvailableVersion)
		case resp.CurrentVersion != "":
			writer.Printf("Automate is up-to-date (%s)\n", resp.CurrentVersion)
		default:
			writer.Printf("Automate is up-to-date (%s)\n", resp.LatestAvailableVersion)
		}
	case api.UpgradeStatusResponse_UPGRADING:
		// Leaving the leading newlines in place to emphasize multi-line output.
		if resp.DesiredVersion != "" {
			writer.Titlef("Automate is upgrading to %s", resp.DesiredVersion)
		} else {
			writer.Titlef("Automate is upgrading to %s", resp.LatestAvailableVersion)
		}

		writer.Title("Services requiring changes:")
		for _, svc := range resp.RemainingServices {
			if svc.Target == nil {
				writer.Bodyf("%s (current: %s) (expected: REMOVED)", svc.Actual.Name, svc.Actual.Release)
			} else if svc.Actual == nil {
				writer.Bodyf("%s (current: NOT RUNNING) (expected: %s)", svc.Target.Name, svc.Target.Release)
			} else {
				writer.Bodyf("%s (current: %s) (expected: %s)", svc.Target.Name, svc.Actual.Release, svc.Target.Release)
			}
		}
	case api.UpgradeStatusResponse_UNKNOWN:
		// I don't think we can get here without hitting the err != nil above first
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Upgrade state could not be determined!",
		)
	}

	return nil
}

func isA1Running() (bool, error) {
	var A1VersionManifestPath = "/opt/delivery/version-manifest.txt"
	ok, err := fileutils.PathExists(A1VersionManifestPath)
	if err != nil {
		// early return and stop the upgrade because of the error
		return false, err
	}

	if !ok {
		// Chef Automate 1 is not installed; early return to continue upgrade
		return false, nil
	}

	err = a1upgrade.AutomateCtlStatus()
	if err != nil {
		// Chef Automate 1 is not running; early return to continue upgrade
		return false, nil
	}

	return true, nil
}

func init() {
	upgradeRunCmd.PersistentFlags().StringVar(
		&upgradeRunCmdFlags.airgap,
		"airgap-bundle",
		"",
		"Path to an airgap install bundle")
	// upgradeRunCmd.PersistentFlags().StringVar(
	// 	&upgradeRunCmdFlags.version,
	// 	"version",
	// 	"",
	// 	"The exact Chef Automate version to install")

	// upgradeRunCmd.PersistentFlags().MarkHidden("version") // nolint: errcheck
	upgradeCmd.AddCommand(upgradeRunCmd)
	upgradeCmd.AddCommand(upgradeStatusCmd)
	RootCmd.AddCommand(upgradeCmd)
}
