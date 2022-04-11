package main

import (
	"context"
	"strconv"
	"strings"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/a1upgrade"
	"github.com/chef/automate/components/automate-deployment/pkg/airgap"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/components/automate-deployment/pkg/majorupgradechecklist"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/lib/io/fileutils"
)

var upgradeCmd = &cobra.Command{
	Use:   "upgrade COMMAND",
	Short: "upgrade automate to the latest version",
}

var upgradeRunCmdFlags = struct {
	airgap               string
	version              string
	upgradefrontends     bool
	upgradebackends      bool
	upgradeairgapbundles bool
	skipDeploy           bool
	isMajorUpgrade       bool
	versionsPath         string
}{}

var upgradeRunCmd = &cobra.Command{
	Use:   "run",
	Short: "Run an upgrade of Chef Automate",
	Long:  "Run an upgrade of Chef Automate",
	RunE:  runUpgradeCmd,
	Args:  cobra.MaximumNArgs(0),
}

var upgradeStatusCmdFlags = struct {
	versionsPath string
}{}

var upgradeStatusCmd = &cobra.Command{
	Use:   "status",
	Short: "Get upgrade status of Chef Automate",
	Long:  "Get upgrade status of Chef Automate",
	RunE:  statusUpgradeCmd,
	Args:  cobra.MaximumNArgs(0),
}

const a1RunningMsg = "You have a running Chef Automate v1 installation. Did you mean to type `chef-automate upgrade-from-v1` (alias for: `chef-automate migrate-from-v1`)?"
const convergeDisabledWarning = `Converge is disabled. This will prevent Automate from upgrading.

To fix this, delete the file "/hab/svc/deployment-service/data/converge_disable".
Otherwise, you may need to run "chef-automate dev start-converge".
`

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

	// check if it is in HA mode
	if isA2HARBFileExist() {
		return runAutomateHAFlow(args, offlineMode)
	}

	if airgap.AirgapInUse() && !offlineMode {
		return status.New(status.InvalidCommandArgsError, "To upgrade a deployment created with an airgap bundle, use --airgap-bundle to specify a bundle to use for the upgrade.")
	}

	if airgap.AirgapInUse() {
		res, err := client.GetAutomateConfig(configCmdFlags.timeout)
		if err != nil {
			return err
		}
		if res.Config.Deployment.GetV1().GetSvc().GetUpgradeStrategy().GetValue() != "none" {
			return status.New(status.InvalidCommandArgsError, "Before running the upgrade, set upgrade_strategy = 'none' and patch the config.")
		}
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

	validatedResp, err := connection.IsValidUpgrade(context.Background(), &api.UpgradeRequest{
		Version:        upgradeRunCmdFlags.version,
		IsMajorUpgrade: upgradeRunCmdFlags.isMajorUpgrade,
		VersionsPath:   upgradeRunCmdFlags.versionsPath,
	})

	if err != nil {
		if !strings.Contains(err.Error(), "unknown method IsValidUpgrade") &&
			!strings.Contains(err.Error(), "Unimplemented desc = unknown service chef.automate.domain.deployment.Deployment") {
			return status.Wrap(
				err,
				status.DeploymentServiceCallError,
				"Request to start upgrade failed",
			)
		}
	} else {
		if validatedResp.CurrentVersion == validatedResp.TargetVersion {
			writer.Println("Chef Automate up-to-date")
			return nil
		}

		pendingPostChecklist, err := GetPendingPostChecklist(validatedResp.CurrentVersion)
		if err != nil {
			return err
		}

		if upgradeRunCmdFlags.isMajorUpgrade && len(pendingPostChecklist) == 0 {
			ci, err := majorupgradechecklist.NewChecklistManager(writer, validatedResp.TargetVersion)
			if err != nil {
				return status.Wrap(
					err,
					status.DeploymentServiceCallError,
					"Request to start upgrade failed",
				)
			}
			err = ci.RunChecklist(majorupgradechecklist.IsExternalPG())
			if err != nil {
				return status.Wrap(
					err,
					status.DeploymentServiceCallError,
					"Request to start upgrade failed",
				)
			}
		}
	}

	resp, err := connection.Upgrade(context.Background(), &api.UpgradeRequest{
		Version:        upgradeRunCmdFlags.version,
		IsMajorUpgrade: upgradeRunCmdFlags.isMajorUpgrade,
		VersionsPath:   upgradeRunCmdFlags.versionsPath,
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

func runAutomateHAFlow(args []string, offlineMode bool) error {
	if (upgradeRunCmdFlags.upgradefrontends && upgradeRunCmdFlags.upgradebackends) || (upgradeRunCmdFlags.upgradefrontends && upgradeRunCmdFlags.upgradeairgapbundles) || (upgradeRunCmdFlags.upgradebackends && upgradeRunCmdFlags.upgradeairgapbundles) {
		return status.New(status.InvalidCommandArgsError, "you cannot use 2 flags together ")
	}
	response, err := writer.Prompt("Installation will get updated to latest version if already not running on newer version press y to agree, n to to disagree? [y/n]")
	if err != nil {
		return err
	}
	if !strings.Contains(response, "y") {
		return errors.New("canceled upgrade")
	}

	if offlineMode {
		writer.Title("Installing airgap install bundle")
		airgapMetaData, err := airgap.Unpack(upgradeRunCmdFlags.airgap)
		if err != nil {
			return status.Annotate(err, status.AirgapUnpackInstallBundleError)
		}
		if upgradeRunCmdFlags.upgradefrontends {
			err := moveAirgapFrontendBundlesOnlyToTransferDir(airgapMetaData, upgradeRunCmdFlags.airgap)
			if err != nil {
				return err
			}
		} else if upgradeRunCmdFlags.upgradebackends {
			err := moveAirgapBackendBundlesOnlyToTransferDir(airgapMetaData, upgradeRunCmdFlags.airgap)
			if err != nil {
				return err
			}
		} else {
			err := moveFrontendBackendAirgapToTransferDir(airgapMetaData, upgradeRunCmdFlags.airgap)
			if err != nil {
				return err
			}
		}
		args = append(args, "-y")
		if upgradeRunCmdFlags.skipDeploy {
			return nil
		}

	} else {
		if upgradeRunCmdFlags.upgradefrontends {
			args = append(args, "--upgrade-frontends", "-y")
		}
		if upgradeRunCmdFlags.upgradebackends {
			args = append(args, "--upgrade-backends", "-y")
		}
		if upgradeRunCmdFlags.upgradeairgapbundles {
			args = append(args, "--upgrade-airgap-bundles", "-y")
		}
		if upgradeRunCmdFlags.skipDeploy {
			args = append(args, "--skip-deploy")
		}
	}
	return executeAutomateClusterCtlCommandAsync("deploy", args, upgradeHaHelpDoc)
}

func statusUpgradeCmd(cmd *cobra.Command, args []string) error {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return err
	}

	resp, err := connection.UpgradeStatus(context.Background(), &api.UpgradeStatusRequest{
		VersionsPath: upgradeStatusCmdFlags.versionsPath,
	})
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to get upgrade status failed",
		)
	}

	if resp.IsConvergeDisable {
		writer.Warn(convergeDisabledWarning)
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
		//Todo(milestone) - update the comparison logic of current version and latest available version
		case resp.CurrentVersion != "":
			if resp.IsAirgapped {
				writer.Printf("Automate is up-to-date with airgap bundle (%s)\n", resp.CurrentVersion)
			} else if resp.CurrentVersion < resp.LatestAvailableVersion {
				writer.Printf("Automate is out-of-date (current version: %s; next available version: %s; is Airgapped: %v)\n",
					resp.CurrentVersion, resp.LatestAvailableVersion, resp.IsAirgapped)
				if !resp.IsConvergeCompatable {
					writer.Printf("Please manually run the major upgrade command to upgrade to %s\n", resp.LatestAvailableVersion)
				}
			} else {
				writer.Printf("Automate is up-to-date (%s)\n", resp.CurrentVersion)
			}
		default:
			if resp.IsAirgapped {
				writer.Printf("Automate is up-to-date with airgap bundle %s\n", resp.LatestAvailableVersion)
			} else {
				writer.Printf("Automate is up-to-date (%s)\n", resp.LatestAvailableVersion)
			}
		}

		pendingPostChecklist, err := GetPendingPostChecklist(resp.CurrentVersion)
		if err != nil {
			return err
		}
		if len(pendingPostChecklist) > 0 {
			writer.Println(majorupgradechecklist.POST_UPGRADE_HEADER)
			for index, msg := range pendingPostChecklist {
				writer.Body("\n" + strconv.Itoa(index+1) + ") " + msg)
			}
		}

	case api.UpgradeStatusResponse_UPGRADING:
		// Leaving the leading newlines in place to emphasize multi-line output.
		if resp.DesiredVersion != "" {
			if resp.IsAirgapped {
				writer.Titlef("Automate is upgrading to airgap bundle %s", resp.DesiredVersion)
			} else {
				writer.Titlef("Automate is upgrading to %s", resp.DesiredVersion)
			}
		} else {
			if resp.IsAirgapped {
				writer.Titlef("Automate is upgrading to airgap bundle %s", resp.LatestAvailableVersion)
			} else {
				writer.Titlef("Automate is upgrading to %s", resp.LatestAvailableVersion)
			}
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
	upgradeRunCmd.PersistentFlags().StringVar(
		&upgradeRunCmdFlags.version,
		"version",
		"",
		"The exact Chef Automate version to install")
	upgradeRunCmd.PersistentFlags().BoolVar(
		&upgradeRunCmdFlags.upgradefrontends,
		"upgrade-frontends",
		false,
		"upgrade Chef Automate HA  frontends version to install")
	upgradeRunCmd.PersistentFlags().BoolVar(
		&upgradeRunCmdFlags.upgradebackends,
		"upgrade-backends",
		false,
		"Update Chef Automate backends version to install")
	upgradeRunCmd.PersistentFlags().BoolVar(
		&upgradeRunCmdFlags.upgradeairgapbundles,
		"upgrade-airgap-bundles",
		false,
		"Update Chef Automate both frontend and backend version to install")
	upgradeRunCmd.PersistentFlags().BoolVar(
		&upgradeRunCmdFlags.skipDeploy,
		"skip-deploy",
		false,
		"will only upgrade and not deploy the bundle")

	upgradeRunCmd.PersistentFlags().BoolVar(
		&upgradeRunCmdFlags.isMajorUpgrade,
		"major",
		false,
		"This flag is only needed for major version upgrades")

	upgradeRunCmd.PersistentFlags().StringVar(
		&upgradeRunCmdFlags.versionsPath, "versions-file", "",
		"Path to versions.json",
	)

	upgradeStatusCmd.PersistentFlags().StringVar(
		&upgradeStatusCmdFlags.versionsPath, "versions-file", "",
		"Path to versions.json",
	)

	if !isDevMode() {
		err := upgradeStatusCmd.PersistentFlags().MarkHidden("versions-file")
		if err != nil {
			writer.Printf("failed configuring cobra: %s\n", err.Error())
		}
		err = upgradeRunCmd.PersistentFlags().MarkHidden("versions-file")
		if err != nil {
			writer.Printf("failed configuring cobra: %s\n", err.Error())
		}
	}

	upgradeCmd.AddCommand(upgradeRunCmd)
	upgradeCmd.AddCommand(upgradeStatusCmd)
	RootCmd.AddCommand(upgradeCmd)
}

func GetPendingPostChecklist(version string) ([]string, error) {

	_, is_major_version := manifest.IsSemVersionFmt(version)

	if is_major_version {
		var err error
		pmc, err := majorupgradechecklist.NewPostChecklistManager(version)
		if err != nil {
			return []string{}, err
		}

		pendingPostChecklist, _ := pmc.ReadPendingPostChecklistFile(majorupgradechecklist.UPGRADE_METADATA, majorupgradechecklist.IsExternalPG())
		return pendingPostChecklist, nil
	}
	return []string{}, nil
}
