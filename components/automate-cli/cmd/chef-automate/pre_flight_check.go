// Copyright © 2017 Chef Software

package main

import (
	"fmt"
	"os"
	"strings"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"

	"github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/a1upgrade"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/lib/version"
)

var preflightCmdFlags = struct {
	airgap             bool
	waitTimeout        int
	configPath         string
	haDeploymentConfig bool
	automate           bool
	chef_server        bool
	frontend           bool
	node               string
}{}

type InfraKeyValues struct {
	Value []string `json:"value"`
	Type  []string `json:"type"`
}

type InfraKeyValue struct {
	Value string `json:"value"`
	Type  string `json:"type"`
}

func init() {
	preflightCheckCmd.PersistentFlags().BoolVar(
		&preflightCmdFlags.airgap,
		"airgap",
		false,
		"Pass this flag when the environment is airgapped")
	preflightCheckCmd.PersistentFlags().BoolVar(
		&preflightCmdFlags.haDeploymentConfig,
		"ha-deployment-config",
		false,
		"Pass this flag to run pre-flight check on automate-HA")
	showConfigCmd.PersistentFlags().SetAnnotation("ha-deployment-config", docs.Compatibility, []string{docs.CompatiblewithHA})
	preflightCheckCmd.PersistentFlags().BoolVar(
		&preflightCmdFlags.frontend,
		"fe",
		false,
		"Pass this flag to run pre-flight check on all Frontend nodes")
	showConfigCmd.PersistentFlags().SetAnnotation("fe", docs.Compatibility, []string{docs.CompatiblewithHA})
	preflightCheckCmd.PersistentFlags().BoolVarP(
		&preflightCmdFlags.frontend,
		"frontend",
		"f",
		false,
		"Pass this flag to run pre-flight check on all Frontend nodes")
	showConfigCmd.PersistentFlags().SetAnnotation("frontend", docs.Compatibility, []string{docs.CompatiblewithHA})
	preflightCheckCmd.PersistentFlags().BoolVarP(
		&preflightCmdFlags.automate,
		"automate",
		"a",
		false,
		"Pass this flag to run pre-flight check on automate node(HA)")
	showConfigCmd.PersistentFlags().SetAnnotation("automate", docs.Compatibility, []string{docs.CompatiblewithHA})
	preflightCheckCmd.PersistentFlags().BoolVar(
		&preflightCmdFlags.automate,
		"a2",
		false,
		"Pass this flag to run pre-flight check on automate node(HA)")
	showConfigCmd.PersistentFlags().SetAnnotation("a2", docs.Compatibility, []string{docs.CompatiblewithHA})
	preflightCheckCmd.PersistentFlags().BoolVarP(
		&preflightCmdFlags.chef_server,
		"chef_server",
		"c",
		false,
		"Pass this flag to run pre-flight check on chef-server node(HA)")
	showConfigCmd.PersistentFlags().SetAnnotation("chef_server", docs.Compatibility, []string{docs.CompatiblewithHA})
	preflightCheckCmd.PersistentFlags().BoolVar(
		&preflightCmdFlags.chef_server,
		"cs",
		false,
		"Pass this flag to run pre-flight check on chef-server node(HA)")
	showConfigCmd.PersistentFlags().SetAnnotation("cs", docs.Compatibility, []string{docs.CompatiblewithHA})
	preflightCheckCmd.PersistentFlags().StringVar(
		&preflightCmdFlags.node,
		"node",
		"",
		"Pass this flag to run pre-flight check on a perticular node node(HA)")
	showConfigCmd.PersistentFlags().SetAnnotation("node", docs.Compatibility, []string{docs.CompatiblewithHA})
	preflightCheckCmd.PersistentFlags().StringVar(
		&preflightCmdFlags.configPath,
		"config",
		"",
		"Optional config file to use")
	RootCmd.AddCommand(preflightCheckCmd)

	preflightCheckCmd.AddCommand(newMigratePreflightCmd())
}

var preflightCheckCmd = &cobra.Command{
	Use:   "preflight-check",
	Short: "Perform preflight check",
	Long:  "Perform preflight check to verify host meets installation criteria.",
	Annotations: map[string]string{
		NoCheckVersionAnnotation: NoCheckVersionAnnotation,
		docs.Tag:                 docs.FrontEnd,
	},
	RunE: runPreflightCheckCmd,
}

func loadMergedConfigForPreflight() (*deployment.AutomateConfig, error) {
	if preflightCmdFlags.configPath == "" {
		return deployment.DefaultAutomateConfig(), nil
	} else {
		return deployment.LoadUserOverrideConfigFile(preflightCmdFlags.configPath)
	}
}

func runPreflightCheckCmd(cmd *cobra.Command, args []string) error {
	if preflightCmdFlags.haDeploymentConfig {
		var configPath = ""
		if len(args) == 0 {
			return errors.Errorf("Config file should be passed with ha-deployment-config flag")
		}
		configPath = args[0]
		infra, err := getInfraDetails(configPath)
		if err != nil {
			return err
		}
		nodeMap := constructAndGetNodeMap(infra)
		sshUtil := NewSSHUtil(&SSHConfig{})

		cmdUtil := NewRemoteCmdExecutor(nodeMap, sshUtil, writer)

		if preflightCmdFlags.automate || preflightCmdFlags.chef_server || preflightCmdFlags.frontend {
			_, err = cmdUtil.Execute()
		} else {
			writer.Println(cmd.UsageString())
		}
		return err
	}
	cfg, err := loadMergedConfigForPreflight()
	if err != nil {
		return err
	}
	if err := client.Preflight(writer, cfg, version.BuildTime, preflightCmdFlags.airgap); err != nil {
		return err
	}
	return nil
}

func getInfraDetails(configPath string) (*AutomateHAInfraDetails, error) {
	var infra *AutomateHAInfraDetails
	var err error
	var deploymentType string
	if isA2HARBFileExist() {
		infra, err = getAutomateHAInfraDetails()
	} else {
		deploymentType, err = getModeFromConfig(configPath)
		if err != nil {
			return nil, errors.Errorf("%v", err)
		}
		infra, err = getInfraDetailsForDeploymentType(deploymentType, configPath)
		if err != nil {
			return nil, errors.Errorf("%v", err)
		}
	}
	return infra, err
}

var migratePreflightCmdFlags = migrateCmdFlagSet{}

func newMigratePreflightCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:     "migrate-from-v1",
		RunE:    runMigratePreflight,
		Short:   "Run preflight checks specific to migrating from Chef Automate v1",
		Aliases: []string{"upgrade-from-v1"},
		Annotations: map[string]string{
			docs.Compatibility: docs.CompatiblewithStandalone,
		},
	}

	cmd.PersistentFlags().StringVarP(
		&migratePreflightCmdFlags.deliverySecretsPath,
		"delivery-secrets",
		"s",
		"/etc/delivery/delivery-secrets.json",
		"Path to delivery-secrets.json")
	cmd.PersistentFlags().StringVarP(
		&migratePreflightCmdFlags.deliveryRunningPath,
		"delivery-running",
		"r",
		"/etc/delivery/delivery-running.json",
		"Path to delivery-running.json")

	cmd.PersistentFlags().BoolVar(
		&migratePreflightCmdFlags.skipBackupCheck,
		"skip-backup-check",
		false,
		"Optionally do not check if your Chef Automate v1 installation has backups configured (default = false)")
	cmd.PersistentFlags().BoolVar(
		&migratePreflightCmdFlags.skipDisasterRecoveryCheck,
		"skip-disaster-recovery-check",
		false,
		"Optionally do not check if your Chef Automate v1 installation has disaster recovery configured (default = false)")
	cmd.PersistentFlags().BoolVar(
		&migratePreflightCmdFlags.skipExternalESCheck,
		"skip-external-es-check",
		false,
		"Optionally do not check if your Chef Automate v1 installation has external Elasticsearch configured (default = false)")
	cmd.PersistentFlags().BoolVar(
		&migratePreflightCmdFlags.skipFIPSCheck,
		"skip-fips-check",
		false,
		"Optionally do not check if your Chef Automate v1 installation has FIPS configured (default = false)")
	cmd.PersistentFlags().BoolVar(
		&migratePreflightCmdFlags.skipSAMLCheck,
		"skip-saml-check",
		false,
		"Optionally do not check if your Chef Automate v1 installation has SAML configured (default = false)")

	cmd.PersistentFlags().BoolVar(
		&migratePreflightCmdFlags.skipWorkflowCheck,
		"skip-workflow-check",
		false,
		"Optionally do not check if your Chef Automate v1 installation has workflow configured (default = false)")

	cmd.PersistentFlags().BoolVar(
		&migratePreflightCmdFlags.airgapPreflight,
		"airgap",
		false,
		"Pass this flag when the environment is airgapped")

	// Chef Server flags (hidden)
	cmd.PersistentFlags().BoolVar(&migratePreflightCmdFlags.enableChefServer,
		"enable-chef-server", false, "Enable Chef Server migration checks")

	cmd.PersistentFlags().StringVar(
		&migratePreflightCmdFlags.chefServerRunningPath,
		"chef-server-running",
		"/etc/opscode/chef-server-running.json",
		"Path to chef-server-running.json")

	err := cmd.PersistentFlags().MarkHidden("enable-chef-server")
	if err != nil {
		fmt.Printf("failed configuring cobra: %s\n", err.Error())
		panic(":(")
	}

	// Also hide this because it's related to the chef-server feature
	err = cmd.PersistentFlags().MarkHidden("chef-server-running")
	if err != nil {
		fmt.Printf("failed configuring cobra: %s\n", err.Error())
		panic(":(")
	}

	return cmd
}

func runMigratePreflight(cmd *cobra.Command, args []string) error {
	u, err := a1upgrade.NewA1Upgrade(
		a1upgrade.WithDeliveryRunning(migratePreflightCmdFlags.deliveryRunningPath),

		a1upgrade.WithDeliverySecrets(migratePreflightCmdFlags.deliverySecretsPath),

		a1upgrade.WithChefServerRunning(migratePreflightCmdFlags.chefServerRunningPath, migratePreflightCmdFlags.enableChefServer),

		a1upgrade.SkipBackupConfiguredCheck(migratePreflightCmdFlags.skipBackupCheck),

		a1upgrade.SkipDisasterRecoveryConfiguredCheck(migratePreflightCmdFlags.skipDisasterRecoveryCheck),

		a1upgrade.SkipExternalESConfiguredCheck(migratePreflightCmdFlags.skipExternalESCheck),

		a1upgrade.SkipFIPSConfiguredCheck(migratePreflightCmdFlags.skipFIPSCheck),

		a1upgrade.SkipSAMLConfiguredCheck(migratePreflightCmdFlags.skipSAMLCheck),

		a1upgrade.SkipWorkflowConfiguredCheck(migratePreflightCmdFlags.skipWorkflowCheck),

		a1upgrade.WithChefServerEnabled(migratePreflightCmdFlags.enableChefServer),

		a1upgrade.WithWorkflowEnabled(migratePreflightCmdFlags.enableWorkflow),
	)

	if err != nil {
		return status.Annotate(err, status.PreflightError)
	}

	// Run the normal install preflight first. The environment
	// variable here tells the script to skip ports that are
	// shared between A1 and A2.
	err = os.Setenv("SKIP_SHARED_PORTS", "1")
	if err != nil {
		return status.Wrap(err, status.PreflightError, "could not set SKIP_SHARED_PORTS environment variable")
	}

	cfg, err := loadMergedConfigForPreflight()
	if err != nil {
		return err
	}

	if err := client.Preflight(writer, cfg, version.BuildTime, migratePreflightCmdFlags.airgapPreflight); err != nil {
		return status.Annotate(err, status.PreflightError)
	}

	p := a1upgrade.NewPreflightRunner(
		writer,
		u.DeliveryRunning,
		u.DeliverySecrets,
		u.EnableChefServer,
		u.EnableWorkflow,
	)
	if err := p.Run(); err != nil {
		return status.Annotate(err, status.PreflightError)
	}

	checker := a1upgrade.NewCompatChecker()

	writer.Title("Checking if your Chef Automate v1 installation uses features that are not compatible with Chef Automate v2...")

	skips := a1upgrade.CompatCheckerSkips{
		BackupCheck:           u.SkipBackupCheck,
		DisasterRecoveryCheck: u.SkipDisasterRecoveryCheck,
		ExternalESCheck:       u.SkipExternalESCheck,
		FIPSCheck:             u.SkipFIPSCheck,
		SAMLCheck:             u.SkipSAMLCheck,
		WorkflowCheck:         u.SkipWorkflowCheck,
	}

	// @afiune delete me when workflow feature is completed, as well as the skip flags
	if u.EnableWorkflow {
		skips.SkipWorkflowCheck()
	}

	err = checker.RunAutomateChecks(u.A1Config, skips)
	if err != nil {
		return status.Wrap(err, status.PreflightError, "Unable to determine if your Chef Automate v1 configuration is suitable for migration.")
	}
	rollupMsg := checker.Msgs.String()
	if checker.Failures == 0 && checker.Warnings == 0 {
		writer.Body("Your Chef Automate v1 config passed compatibility checks.")
	}

	if checker.Failures > 0 {
		sum := checker.Failures + checker.Warnings
		builder := strings.Builder{}
		fmt.Fprintf(&builder, "We found %d potential compatibility issue(s) between your Chef Automate v1 configuration with Chef Automate v2:\n", sum)
		fmt.Fprintf(&builder, "%s\n", rollupMsg)
		builder.WriteString("Please address these issues to continue with your migration to Chef Automate v2")
		return status.WithRecovery(
			status.New(status.PreflightError, "Migration compatibility checks failed"),
			builder.String(),
		)
	}

	if checker.Warnings > 0 {
		// TODO: need to adjust the wording to explain these are low-priority checks that _could_ be skipped...
		builder := strings.Builder{}
		fmt.Fprintf(&builder, "We found %d potential compatibility issue(s) between your Chef Automate v1 configuration and Chef Automate v2:\n", checker.Warnings)
		fmt.Fprintf(&builder, "%s\n", rollupMsg)
		builder.WriteString("Please address these issues to continue with your migration to Chef Automate v2")
		return status.WithRecovery(
			status.New(status.PreflightError, "Migration compatibility checks failed"),
			builder.String(),
		)
	}

	err = runChefServerChecks(u)
	if err != nil {
		return err
	}

	err = runWorkflowChecks(u)
	if err != nil {
		return err
	}

	return nil

}

func runWorkflowChecks(u *a1upgrade.A1Upgrade) error {
	if !migratePreflightCmdFlags.enableWorkflow {
		return nil
	}

	writer.Title("Checking if your Workflow Server installation uses features that are not compatible with Chef Automate v2...")
	checker := a1upgrade.NewCompatChecker()
	a1Config := u.A1Config
	err := checker.RunWorkflowChecks(a1Config)
	if err != nil {
		return err
	}

	if checker.Failures > 0 {
		rollupMsg := checker.Msgs.String()
		sum := checker.Failures + checker.Warnings
		builder := strings.Builder{}
		fmt.Fprintf(&builder, "We found %d issue(s) with your Chef Automate Workflow configuration preventing it from being included in the Chef Automate migration:\n", sum)
		fmt.Fprintf(&builder, "%s\n", rollupMsg)
		builder.WriteString("Please address these issues to continue with your migration to Chef Automate v2")
		return status.WithRecovery(
			status.New(status.PreflightError, "Migration compatibility checks failed"),
			builder.String(),
		)
	}

	writer.Body("Your Chef Server config passed compatibility checks.")

	return nil
}

func runChefServerChecks(u *a1upgrade.A1Upgrade) error {
	if !migratePreflightCmdFlags.enableChefServer {
		return nil
	}

	writer.Title("Checking if your Chef Server installation uses features that are not compatible with Chef Automate v2...")
	checker := a1upgrade.NewCompatChecker()
	a1Config := u.A1Config
	err := checker.RunChefServerChecks(a1Config)
	if err != nil {
		return err
	}

	// NOTE: if you add a check that is a WARNING, you need to add the warning messaging code here

	if checker.Failures > 0 {
		rollupMsg := checker.Msgs.String()
		sum := checker.Failures + checker.Warnings
		builder := strings.Builder{}
		fmt.Fprintf(&builder, "We found %d issue(s) with your Chef Server configuration preventing it from being included in the Chef Automate migration:\n", sum)
		fmt.Fprintf(&builder, "%s\n", rollupMsg)
		builder.WriteString("Please address these issues to continue with your migration to Chef Automate v2")
		return status.WithRecovery(
			status.New(status.PreflightError, "Upgrade compatibility checks failed"),
			builder.String(),
		)
	}

	writer.Body("Your Chef Server config passed compatibility checks.")

	return nil
}

func populateInfraDetails(config ExistingInfraConfigToml) *AutomateHAInfraDetails {

	var infra AutomateHAInfraDetails
	infra.Outputs.SSHKeyFile = InfraKeyValue{
		Value: config.Architecture.ConfigInitials.SSHKeyFile,
	}
	infra.Outputs.SSHPort = InfraKeyValue{
		Value: config.Architecture.ConfigInitials.SSHPort,
	}
	infra.Outputs.SSHUser = InfraKeyValue{
		Value: config.Architecture.ConfigInitials.SSHUser,
	}
	infra.Outputs.AutomatePrivateIps = InfraKeyValues{
		Value: config.ExistingInfra.Config.AutomatePrivateIps,
	}
	infra.Outputs.ChefServerPrivateIps = InfraKeyValues{
		Value: config.ExistingInfra.Config.ChefServerPrivateIps,
	}
	return &infra
}

func getInfraDetailsForDeploymentType(deploymentType string, configPath string) (*AutomateHAInfraDetails, error) {
	var infra *AutomateHAInfraDetails
	var err error
	if deploymentType == EXISTING_INFRA_MODE {
		config, err := getExistingInfraConfig(configPath)
		if err != nil {
			return nil, errors.Errorf("%v", err)
		}
		infra = populateInfraDetails(*config)
	}

	if deploymentType == AWS_MODE {
		infra, err = getAutomateHAInfraDetails()
		if err != nil {
			return nil, errors.Errorf("%v", err)
		}
	}
	return infra, nil
}

func constructAndGetNodeMap(infra *AutomateHAInfraDetails) *NodeTypeAndCmd {
	frontendCommand := fmt.Sprint(PRE_FLIGHT_CHECK)
	inputFilePath := "/usr/bin/chef-automate"
	nodeMap := &NodeTypeAndCmd{
		Frontend: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      frontendCommand,
				WaitTimeout:              preflightCmdFlags.waitTimeout,
				ErrorCheckEnableInOutput: true,
				Single:                   false,
				NodeIps:                  []string{preflightCmdFlags.node},
				InputFiles:               []string{inputFilePath},
				Outputfiles:              []string{},
				NodeType:                 preflightCmdFlags.frontend,
			},
		},
		Automate: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      frontendCommand,
				WaitTimeout:              preflightCmdFlags.waitTimeout,
				ErrorCheckEnableInOutput: true,
				Single:                   false,
				NodeIps:                  []string{preflightCmdFlags.node},
				InputFiles:               []string{inputFilePath},
				Outputfiles:              []string{},
				NodeType:                 preflightCmdFlags.automate,
			},
		},
		ChefServer: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      frontendCommand,
				WaitTimeout:              preflightCmdFlags.waitTimeout,
				ErrorCheckEnableInOutput: true,
				Single:                   false,
				NodeIps:                  []string{preflightCmdFlags.node},
				InputFiles:               []string{inputFilePath},
				Outputfiles:              []string{},
				NodeType:                 preflightCmdFlags.chef_server,
			},
		},
		Infra: infra,
	}
	return nodeMap
}
