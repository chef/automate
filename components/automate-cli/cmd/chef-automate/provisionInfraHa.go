package main

import (
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/spf13/cobra"
)

func newProvisionInfraCmd() *cobra.Command {
	var provisionInfraCmd = &cobra.Command{
		Use:   "provision-infra",
		Short: "Provision Automate HA infra.",
		Long:  "Provision infra for Automate HA deployment.",
		Args:  cobra.RangeArgs(0, 1),
		RunE:  runProvisionInfraCmd,
	}

	provisionInfraCmd.PersistentFlags().StringVar(
		&deployCmdFlags.channel,
		"channel",
		"",
		"Release channel to deploy all services from")
	provisionInfraCmd.PersistentFlags().StringVar(
		&deployCmdFlags.airgap,
		"airgap-bundle",
		"",
		"Path to an airgap install bundle")

	return provisionInfraCmd
}

func runProvisionInfraCmd(cmd *cobra.Command, args []string) error {
	var configPath = ""
	if len(args) > 0 {
		configPath = args[0]
	}
	deployer, err := getDeployer(configPath)
	if err != nil {
		return status.Wrap(err, status.ConfigError, invalidConfig)
	}
	if deployer != nil {
		return deployer.doProvisionJob(args)
	} else {
		return status.New(status.InvalidCommandArgsError, provisionInfraHelpDocs)
	}
}

func init() {
	RootCmd.AddCommand(newProvisionInfraCmd())
}
