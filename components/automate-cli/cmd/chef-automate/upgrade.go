package main

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"time"

	"github.com/briandowns/spinner"
	"github.com/chef/automate/api/config/deployment"
	opensearch "github.com/chef/automate/api/config/opensearch"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/cmd/chef-automate/migrator/migratorv4"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/a1upgrade"
	"github.com/chef/automate/components/automate-deployment/pkg/airgap"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/components/automate-deployment/pkg/inspector/upgradeinspectorv4"
	"github.com/chef/automate/components/automate-deployment/pkg/majorupgradechecklist"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/toml"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/fatih/color"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
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
	skipStorageCheck     bool
	osDestDataDir        string
	versionsPath         string
	acceptMLSA           bool
	upgradeHAWorkspace   string
	saas                 bool
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
	Annotations: map[string]string{
		docs.Tag: docs.FrontEnd,
	},
}

const disableMaintenanceModeCmd = `chef-automate maintenance off`
const patchConfigCommand = `chef-automate config patch`
const disableMaintenanceModeMsg = `Please disable the maintenance mode to allow ingestion by using ` + disableMaintenanceModeCmd
const a1RunningMsg = "You have a running Chef Automate v1 installation. Did you mean to type `chef-automate upgrade-from-v1` (alias for: `chef-automate migrate-from-v1`)?"
const convergeDisabledWarning = `Converge is disabled. This will prevent Automate from upgrading.

To fix this, delete the file "/hab/svc/deployment-service/data/converge_disable".
Otherwise, you may need to run "chef-automate dev start-converge".
`
const openSearchConfigFile = "opensearch_config.toml"

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
		// Restart Deployment service to update the manifest.json
		err = restartDeploymentService()
		if err != nil {
			return status.Annotate(err, status.RestartDeploymentServiceError)
		}
	}

	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return err
	}
	var major string
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
		PrintVersions(writer, validatedResp.CurrentVersion, validatedResp.TargetVersion)
		if validatedResp.CurrentVersion == validatedResp.TargetVersion {
			writer.Println("Chef Automate up-to-date")
			return nil
		}

		pendingPostChecklist, err := GetPendingPostChecklist(validatedResp.CurrentVersion)
		if err != nil {
			return err
		}

		if upgradeRunCmdFlags.isMajorUpgrade && len(pendingPostChecklist) == 0 {
			/* err = majorupgradechecklist.StoreESSettings()
			if err != nil {
				writer.Println("Failed to read and store search settings")
			} */
			major, _ = majorupgradechecklist.GetMajorVersion(validatedResp.TargetVersion)
			switch major {
			case "3":
				ci, err := majorupgradechecklist.NewChecklistManager(writer, validatedResp.TargetVersion)
				if err != nil {
					return status.Wrap(
						err,
						status.DeploymentServiceCallError,
						"Request to start upgrade failed",
					)
				}

				flags := majorupgradechecklist.ChecklistUpgradeFlags{
					SkipStorageCheck: upgradeRunCmdFlags.skipStorageCheck,
					OsDestDataDir:    upgradeRunCmdFlags.osDestDataDir,
				}
				err = ci.RunChecklist(configCmdFlags.timeout, flags)
				if err != nil {
					exec.Command("/bin/sh", "-c", disableMaintenanceModeCmd).Output()
					return status.Wrap(
						err,
						status.DeploymentServiceCallError,
						"Request to start upgrade failed",
					)
				}
			case "4":
				upgradeInspector := upgradeinspectorv4.NewUpgradeInspectorV4(writer, upgradeinspectorv4.NewUpgradeV4Utils(), &fileutils.FileSystemUtils{}, configCmdFlags.timeout)
				isError := upgradeInspector.RunUpgradeInspector(upgradeRunCmdFlags.osDestDataDir, upgradeRunCmdFlags.skipStorageCheck)
				if isError {
					return nil
				}
			default:
				return status.Errorf(status.UpgradeError, "invalid major version")
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
		if upgradeRunCmdFlags.isMajorUpgrade {
			switch major {
			case "4":
				isExternalOpenSearch := majorupgrade_utils.IsExternalElasticSearch(configCmdFlags.timeout)
				if isExternalOpenSearch {
					err := postUpgradingExternal(resp)
					if err != nil {
						return err
					}
				} else {
					err := postUpgradingEmbedded(resp)
					if err != nil {
						return err
					}
				}
			default:
				writer.Println("Upgrading Chef Automate")
			}
		}
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

func postUpgradingEmbedded(resp *api.UpgradeResponse) error {
	writer.Println(fmt.Sprintf("Upgrading Chef Automate from version %s to %s", resp.PreviousVersion, resp.NextVersion))
	writer.Println("This might take around 15 to 20 min")
	writer.Println("")
	writer.Println("Once upgrade is complete, You will get an option to migrate data from Elasticsearch to OpenSearch.")
	writer.Println("Maintenance mode will be turned off after migration is complete.")
	writer.Println("")
	writer.Println("To check the upgrade status use " + color.New(color.Bold).Sprint("$ chef-automate upgrade status"))
	return nil
}

func postUpgradingExternal(resp *api.UpgradeResponse) error {
	writer.Println(fmt.Sprintf("\nUpgrading Chef Automate from version %s to version %s.", resp.PreviousVersion, resp.NextVersion))
	writer.Println("")
	msg := fmt.Sprintf(`----------------------------------------------------------------------
IMPORTANT

To establish connection between automate and OpenSearch database,
it is required to patch the configuration file with correct values.

We have created a sample config file for configuring external OpenSearch:
%s

Once upgrade is complete, you must update this file with actual external OpenSearch connection configurations
and then run the below patch command to update the configurations:
%s
----------------------------------------------------------------------
`, color.New(color.Bold).Sprint(openSearchConfigFile), color.New(color.Bold).Sprint(fmt.Sprintf("$ %s %s", patchConfigCommand, openSearchConfigFile)))
	writer.Println(msg)

	file, err := os.Create(openSearchConfigFile)
	if err != nil {
		return err
	}
	defer file.Close()

	const openSearchConfig = `[global.v1.external.opensearch]
  enable = true
  nodes = ["https://opensearch1.example:9200", "https://opensearch2.example:9200"]

# Uncomment and fill out if using external opensearch with SSL and/or basic auth
[global.v1.external.opensearch.auth]
  scheme = "basic_auth"
[global.v1.external.opensearch.auth.basic_auth]
## Create this opensearch user before starting the Chef Automate deployment;
## Chef Automate assumes it exists.
  username = "<admin username>"
  password = "<admin password>"
# Use below configuration only if using HTTPS connection
[global.v1.external.opensearch.ssl]
# Specify either a root_cert or a root_cert_file
  root_cert = """$(cat </path/to/cert_file.crt>)"""
# server_name = "<opensearch server name>"
  
# Uncomment and fill out if using external OpenSearch that uses hostname-based routing/load balancing
# [esgateway.v1.sys.ngx.http]
#  proxy_set_header_host = "<Your External OpenSearch Hostname>:<Port-No>"

# Uncomment and add to change the ssl_verify_depth for the root cert bundle
#   ssl_verify_depth = "5"
`
	_, err = file.WriteString(openSearchConfig)
	if err != nil {
		return err
	}
	writer.Println("To check the upgrade status use " + color.New(color.Bold).Sprint("$ chef-automate upgrade status"))
	return nil
}

func PrintVersions(writer *cli.Writer, currentVersion, targetVersion string) {
	writer.Println("Current version: " + currentVersion)
	writer.Println("Target version: " + targetVersion)
	writer.Println("")
}

// restartDeploymentService will kill the Pid of Deployment Service and then Hab will restart this service
func restartDeploymentService() error {
	writer.Println("Trying to restart Deployment Service...")
	res, err := getStatus()
	if err != nil {
		return err
	}
	isDeploymentServiceKilled := false
	for _, s := range res.ServiceStatus.Services {
		if s.Name == "deployment-service" {
			deploymentServiceProcess := os.Process{Pid: int(s.Pid)}
			err := deploymentServiceProcess.Kill()
			if err != nil {
				return err
			}
			isDeploymentServiceKilled = true
			writer.Println("Deployment service is stopped")
		}
	}
	if !isDeploymentServiceKilled {
		return errors.New("Failed to stop Deployment Service")
	}
	time.Sleep(10 * time.Second)
	retryLimit := 30
	for i := 0; i < retryLimit; i++ {
		res, err := getStatus()
		if err != nil {
			return err
		}
		for _, s := range res.ServiceStatus.Services {
			if s.Name == "deployment-service" {
				if s.State == api.ServiceState_OK {
					writer.Println("Deployment Service is healthy now")
					return nil
				}
				writer.Println("Waiting for Deployment Service to be healthy")
				time.Sleep(10 * time.Second)
			}
		}
	}
	return errors.New("Deployment service is not healthy after restarting.")
}

func runAutomateHAFlow(args []string, offlineMode bool) error {
	isManagedServices := isManagedServicesOn()
	if isManagedServices && !upgradeRunCmdFlags.upgradefrontends {
		return status.Annotate(
			errors.New("Backend can not be upgraded incase of managed services, please use with flag --upgrade-frontends"), status.InvalidCommandArgsError)
	}
	if (upgradeRunCmdFlags.upgradefrontends && upgradeRunCmdFlags.upgradebackends) || (upgradeRunCmdFlags.upgradefrontends && upgradeRunCmdFlags.upgradeairgapbundles) || (upgradeRunCmdFlags.upgradebackends && upgradeRunCmdFlags.upgradeairgapbundles) {
		return status.New(status.InvalidCommandArgsError, "you cannot use 2 flags together ")
	}
	if !upgradeRunCmdFlags.acceptMLSA {
		response, err := writer.Prompt("press y to start upgrade, n to to abort? [y/n]")
		if err != nil {
			return err
		}
		if !strings.Contains(response, "y") {
			return errors.New("canceled upgrade")
		}
	}
	modeOfDeployment := getModeOfDeployment()
	if modeOfDeployment == EXISTING_INFRA_MODE {

		infra, err := getAutomateHAInfraDetails()
		if err != nil {
			return err
		}
		sshConfig := &SSHConfig{
			sshUser:    infra.Outputs.SSHUser.Value,
			sshKeyFile: infra.Outputs.SSHKeyFile.Value,
			sshPort:    infra.Outputs.SSHPort.Value,
		}
		sshUtil := NewSSHUtil(sshConfig)
		configPuller := NewPullConfigs(infra, sshUtil)
		config, err := configPuller.generateInfraConfig()
		if err != nil {
			return err
		}
		finalTemplate := renderSettingsToA2HARBFile(existingNodesA2harbTemplate, config)
		writeToA2HARBFile(finalTemplate, initConfigHabA2HAPathFlag.a2haDirPath+"a2ha.rb")
		writer.Println("a2ha.rb has regenerated...")
	} else if modeOfDeployment == AWS_MODE {

		infra, err := getAutomateHAInfraDetails()
		if err != nil {
			return err
		}
		sshConfig := &SSHConfig{
			sshUser:    infra.Outputs.SSHUser.Value,
			sshKeyFile: infra.Outputs.SSHKeyFile.Value,
			sshPort:    infra.Outputs.SSHPort.Value,
		}
		sshUtil := NewSSHUtil(sshConfig)
		configPuller := NewPullConfigs(infra, sshUtil)
		config, err := configPuller.generateAwsConfig()
		if err != nil {
			return err
		}
		finalTemplate := renderSettingsToA2HARBFile(awsA2harbTemplate, config)
		writeToA2HARBFile(finalTemplate, initConfigHabA2HAPathFlag.a2haDirPath+"a2ha.rb")
		writer.Println("a2ha.rb has regenerated...")
	}

	if offlineMode {
		// Always upgrade the workspace
		upgradeRunCmdFlags.upgradeHAWorkspace = "yes"
		uperr, upgraded := upgradeWorspace(upgradeRunCmdFlags.airgap, upgradeRunCmdFlags.saas, upgradeRunCmdFlags.upgradefrontends, upgradeRunCmdFlags.upgradebackends)
		if uperr != nil {
			return status.Annotate(uperr, status.UpgradeError)
		}
		if !upgraded {
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
		//// NOT NEEDED will remove it in future, after further discussion,
		//// as by core nature A2HA need airgap bundle to deploy,
		//// so we can ask user it to provide airgap bundle,
		//// which will be more convient

		/* if upgradeRunCmdFlags.upgradeairgapbundles {
			args = append(args, "--upgrade-airgap-bundles", "-y")
		}
		if upgradeRunCmdFlags.skipDeploy {
			args = append(args, "--skip-deploy")
		} */
	}
	return executeAutomateClusterCtlCommandAsync("deploy", args, upgradeHaHelpDoc, true)
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
		writer.Warn(disableMaintenanceModeMsg)
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
				printUpgradeStatusMsg(resp)
			} else if resp.CurrentVersion < resp.LatestAvailableVersion {
				isMajor := !resp.IsConvergeCompatable
				PrintAutomateOutOfDate(writer, resp.CurrentVersion, resp.LatestAvailableVersion, isMajor)
				// writer.Printf("Automate is out-of-date (current version: %s; next available version: %s; is Airgapped: %v)\n",
				// 	resp.CurrentVersion, resp.LatestAvailableVersion, resp.IsAirgapped)
				// if !resp.IsConvergeCompatable {
				// 	writer.Printf("Please manually run the major upgrade command to upgrade to %s\n", resp.LatestAvailableVersion)
				// }
			} else {
				printUpgradeStatusMsg(resp)
			}
		default:
			printUpgradeStatusMsg(resp)
		}

		err := postUpgradeStatus(resp)
		if err != nil {
			return err
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
			fmt.Sprintf("Upgrade state could not be determined! /n/n %s", disableMaintenanceModeMsg),
		)
	}

	return nil
}

func postUpgradeStatus(resp *api.UpgradeStatusResponse) error {
	major, _ := majorupgradechecklist.GetMajorVersion(resp.CurrentVersion)
	isExternalOpenSearch := majorupgrade_utils.IsExternalElasticSearch(configCmdFlags.timeout)
	switch major {
	case "4":
		migrator := migratorv4.NewMigratorV4(writer, migratorv4.NewMigratorV4Utils(), &fileutils.FileSystemUtils{}, 10, time.Second)
		isSkipped, _ := migrator.IsMigrationPermanentlySkipped()
		if !isSkipped {
			pendingPostChecklist, err := GetPendingPostChecklist(resp.CurrentVersion)
			if err != nil {
				return err
			}
			if len(pendingPostChecklist) > 0 {
				if isExternalOpenSearch {
					return postUpgradeStatusExternal(resp)
				}
				return postUpgradeStatusEmbedded(resp)
			}
		}
	case "3":
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
		err = majorupgradechecklist.SetSeenTrueForExternal()
		if err != nil {
			return err
		}
	}
	return nil
}

// Print the automate upgrade status when automate is up to date
func printUpgradeStatusMsg(resp *api.UpgradeStatusResponse) {
	writer.Println(color.New(color.FgGreen).Sprint("------------------------------------------------------------------------------------"))
	if resp.IsAirgapped {
		writer.Println(color.New(color.FgGreen).Sprintf("✔ Chef Automate upgraded to airgap bundle version: %s", resp.CurrentVersion))
	} else {
		writer.Println(color.New(color.FgGreen).Sprintf("✔ Chef Automate upgraded to version: %s", resp.CurrentVersion))
	}
	writer.Println(fmt.Sprintf("  Find out what's new in version (%s) by visiting ", resp.CurrentVersion))
	writer.Println("  visit" + color.New(color.FgBlue).Sprint(fmt.Sprintf(" https://docs.chef.io/release_notes_automate/#%s ", resp.CurrentVersion)))
	writer.Println(color.New(color.FgGreen).Sprint("------------------------------------------------------------------------------------"))
	writer.Println("")
}

func startSpinner(msg string) *spinner.Spinner {
	spinner := writer.NewSpinner()
	spinner.Suffix = fmt.Sprintf("  %s", msg)
	spinner.Start()
	time.Sleep(time.Second)
	return spinner
}

func stopSpinner(spinner *spinner.Spinner, msg string) {
	spinner.FinalMSG = fmt.Sprintf("%s  %s", color.New(color.FgGreen).Sprint("✔"), msg)
	spinner.Stop()
	writer.Println("")
}

// prompt user for confirmation
func promptUser(message string) (bool, error) {
	return writer.Confirm(message)
}

func startMigration() error {
	migrator := migratorv4.NewMigratorV4(writer, migratorv4.NewMigratorV4Utils(), &fileutils.FileSystemUtils{}, 10, time.Second)
	migrator.RunMigrationFlow(true)
	return nil
}

func contains(s []*api.ServiceState, e string) bool {
	for _, a := range s {
		if a.Name == e {
			return true
		}
	}
	return false
}

// postUpgradeStatusExternal - Handle case for external opensearch after upgrade status
func postUpgradeStatusExternal(resp *api.UpgradeStatusResponse) error {

	svcList, err := majorupgrade_utils.GetAutomateSvcList()
	if err != nil {
		return err
	}
	if !contains(svcList, "automate-opensearch") {
		return nil
	}

	isUserConsent, err := promptUser("Have you updated your " + color.New(color.Bold).Sprint(openSearchConfigFile) + " with actual external OpenSearch connection configurations?")
	writer.Println("")
	if err != nil {
		return err
	}

	//Handle the case where user has updated the opensearch_config.toml i.e. `y` case
	if isUserConsent {
		spinner := startSpinner("Updating external OpenSearch configurations")
		//TODO: Need to add opensearch_config.toml path
		_, err := exec.Command("/bin/sh", "-c", fmt.Sprintf("%s %s", patchConfigCommand, openSearchConfigFile)).Output()
		if err != nil {
			return err
		}
		stopSpinner(spinner, "External OpenSearch configurations updated successfully.")

		err = majorupgradechecklist.SetSeenTrueForExternal()
		if err != nil {
			return err
		}

		_, _, err = majorupgrade_utils.SetMaintenanceMode(configCmdFlags.timeout, false)
		if err != nil {
			return err
		}
		writer.Println(fmt.Sprintf("%s  Maintenance mode turned OFF successfully", color.New(color.FgGreen).Sprint("✔")))
		return nil
	}

	// Handle the case where user has not updated the opensearch_config.toml i.e. `n` case
	writer.Println("After the upgrade, you must update opensearch_config.toml with actual external OpenSearch connection configurations and then run the below patch command to update the configurations:")
	writer.Println(color.New(color.Bold).Sprint("$ chef-automate config patch opensearch_config.toml"))
	writer.Println("")
	_, _, err = majorupgrade_utils.SetMaintenanceMode(configCmdFlags.timeout, false)
	if err != nil {
		return err
	}
	writer.Println(fmt.Sprintf("%s  Maintenance mode turned OFF successfully", color.New(color.FgGreen).Sprint("✔")))
	return nil
}

// postUpgardeStatusEmbedded - Handle case for embedded opensearch after upgrade status
func postUpgradeStatusEmbedded(resp *api.UpgradeStatusResponse) error {

	isMigrationConsent, err := promptUser("Do you wish to migrate the Elasticsearch data to OpenSearch now?")
	writer.Println("")
	if err != nil {
		return err
	}

	//Handle the case where user wish to migrate the Elasticsearch data to OpenSearch i.e. `y`
	if isMigrationConsent {
		return startMigration()
	}

	//Handle the case where user does not wish to migrate the Elasticsearch data to OpenSearch i.e. `n` case
	writer.Println(color.New(color.FgYellow).Sprint("!") + " [" + color.New(color.FgYellow).Sprint("Warning") + "] " + "  If the data migration is performed at a later point any data collected since the upgrade will be lost.")
	writer.Println("")
	writer.Println("To migrate data later on, use this command")
	writer.Println(color.New(color.Bold).Sprint("$ chef-automate post-major-upgrade migrate --data=es"))
	writer.Println("")
	writer.Println("To skip data migration permanently, use this command")
	writer.Println(color.New(color.Bold).Sprint("$ chef-automate post-major-upgrade migrate --data=es --skip-migration"))
	writer.Println("")
	isSkipConsent, err := promptUser("Are you sure you want to skip the data migration?")
	if err != nil {
		return err
	}
	//Handle the case where user wishes to skip migration i.e. `y` case
	if isSkipConsent {
		writer.Println("Data migration skipped")
		writer.Println("")
		_, _, err := majorupgrade_utils.SetMaintenanceMode(configCmdFlags.timeout, false)
		if err != nil {
			return err
		}
		writer.Println(fmt.Sprintf("%s Maintenance mode turned OFF successfully", color.New(color.FgGreen).Sprint("✔")))
		return nil
	}
	// Handle the case where user does not wish to skip migration i.e. `n` case
	return startMigration()
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
	upgradeRunCmd.PersistentFlags().SetAnnotation("version", docs.Compatibility, []string{docs.CompatiblewithStandalone})
	upgradeRunCmd.PersistentFlags().BoolVar(
		&upgradeRunCmdFlags.upgradefrontends,
		"upgrade-frontends",
		false,
		"upgrade Chef Automate HA  frontends version to install")
	upgradeRunCmd.PersistentFlags().SetAnnotation("upgrade-frontends", docs.Compatibility, []string{docs.CompatiblewithHA})
	upgradeRunCmd.PersistentFlags().BoolVar(
		&upgradeRunCmdFlags.upgradebackends,
		"upgrade-backends",
		false,
		"Update Chef Automate backends version to install")
	upgradeRunCmd.PersistentFlags().SetAnnotation("upgrade-backends", docs.Compatibility, []string{docs.CompatiblewithHA})
	upgradeRunCmd.PersistentFlags().BoolVar(
		&upgradeRunCmdFlags.upgradeairgapbundles,
		"upgrade-airgap-bundles",
		false,
		"Update Chef Automate both frontend and backend version to install")
	upgradeRunCmd.PersistentFlags().SetAnnotation("upgrade-airgap-bundles", docs.Compatibility, []string{docs.CompatiblewithHA})
	upgradeRunCmd.PersistentFlags().BoolVar(
		&upgradeRunCmdFlags.skipDeploy,
		"skip-deploy",
		false,
		"will only upgrade and not deploy the bundle")
	upgradeRunCmd.PersistentFlags().SetAnnotation("skip-deploy", docs.Compatibility, []string{docs.CompatiblewithHA})
	upgradeRunCmd.PersistentFlags().BoolVarP(
		&upgradeRunCmdFlags.acceptMLSA,
		"auto-approve",
		"y",
		false,
		"Do not prompt for confirmation; accept defaults and continue")
	upgradeRunCmd.PersistentFlags().SetAnnotation("auto-approve", docs.Compatibility, []string{docs.CompatiblewithHA})

	upgradeRunCmd.PersistentFlags().StringVarP(
		&upgradeRunCmdFlags.upgradeHAWorkspace,
		"workspace-upgrade",
		"w",
		"",
		"Do not prompt for confirmation to accept workspace upgrade")
	upgradeRunCmd.PersistentFlags().SetAnnotation("workspace-upgrade", docs.Compatibility, []string{docs.CompatiblewithHA})

	upgradeRunCmd.PersistentFlags().BoolVar(
		&upgradeRunCmdFlags.isMajorUpgrade,
		"major",
		false,
		"This flag is only needed for major version upgrades")
	upgradeRunCmd.PersistentFlags().SetAnnotation("major", docs.Compatibility, []string{docs.CompatiblewithStandalone})

	upgradeRunCmd.PersistentFlags().StringVar(
		&upgradeRunCmdFlags.versionsPath, "versions-file", "",
		"Path to versions.json",
	)

	upgradeRunCmd.PersistentFlags().BoolVarP(
		&upgradeRunCmdFlags.saas,
		"saas",
		"",
		false,
		"Flag for saas setup")
	upgradeRunCmd.PersistentFlags().SetAnnotation("saas", docs.Compatibility, []string{docs.CompatiblewithHA})

	upgradeRunCmd.PersistentFlags().BoolVarP(
		&upgradeRunCmdFlags.skipStorageCheck,
		"skip-storage-check",
		"",
		false,
		"Flag for skipping disk space check during upgrade")
	upgradeRunCmd.PersistentFlags().SetAnnotation("skip-storage-check", docs.Compatibility, []string{docs.CompatiblewithStandalone})

	upgradeRunCmd.PersistentFlags().StringVar(
		&upgradeRunCmdFlags.osDestDataDir,
		"os-dest-data-dir",
		"",
		"Flag for providing custom os destination data directory")
	upgradeRunCmd.PersistentFlags().SetAnnotation("os-dest-data-dir", docs.Compatibility, []string{docs.CompatiblewithStandalone})

	upgradeStatusCmd.PersistentFlags().StringVar(
		&upgradeStatusCmdFlags.versionsPath, "versions-file", "",
		"Path to versions.json",
	)

	upgradeRunCmd.SetHelpFunc(func(command *cobra.Command, strings []string) {
		// Hide flag for this command
		command.Flags().MarkHidden("saas")
		// Call parent help func
		command.Parent().HelpFunc()(command, strings)
	})

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

		pendingPostChecklist, _ := pmc.ReadPendingPostChecklistFile(fileutils.GetHabRootPath() + majorupgrade_utils.UPGRADE_METADATA)
		return pendingPostChecklist, nil
	}
	return []string{}, nil
}

func GetopenSearchConfig() {
	res := deployment.DefaultAutomateConfig()

	con := res.GetOpensearch()
	if con != nil {
		opensearchV1 := &OpenSearch_v1{
			V1: con.V1,
		}
		opensearchModel := &OpenSearchModel{
			Opensearch: opensearchV1,
		}
		t, err := toml.Marshal(opensearchModel)
		if err != nil {
			return
		}

		writer.Println("This is your Default OpenSearch Config")
		writer.Println(string(t))

	}
}

type OpenSearchModel struct {
	Opensearch *OpenSearch_v1 `json:"opensearch,omitempty" toml:"opensearch,omitempty" mapstructure:"opensearch,omitempty"`
}

type OpenSearch_v1 struct {
	V1 *opensearch.ConfigRequest_V1 `json:"v1,omitempty" toml:"v1,omitempty" mapstructure:"v1,omitempty"`
}

const (
	msgAutomateOutOfDate = `Automate is out-of-date !!`
	msgInfoMajor         = `
Please ensure you are using latest CLI version and then run the command:
  $ chef-automate upgrade run --major command to upgrade to latest version

Visit https://docs.chef.io/automate/major_upgrade_4.x for more information`
)

func PrintAutomateOutOfDate(writer *cli.Writer, currentVersion, latestVersion string, isMajor bool) {
	writer.Println(msgAutomateOutOfDate)
	writer.Println("Current version: " + currentVersion)
	writer.Println("Latest upgradable version: " + latestVersion)
	if isMajor {
		writer.Println(msgInfoMajor)
	}
	writer.Println("")
}
