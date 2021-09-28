package main

import (
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/spf13/cobra"
)

func newProvisionInfraCmd() *cobra.Command {
	var provisionInfraCmd = &cobra.Command{
		Use:   "provision-infra",
		Short: "Provison automate HA infra.",
		Long:  "Provison automate HA infra for automate HA deployment.",
		Args:  cobra.RangeArgs(0, 1),
		RunE:  runProvisionInfraCmd,
	}

	return provisionInfraCmd
}

func runProvisionInfraCmd(cmd *cobra.Command, args []string) error {
	deployer, err := getDeployer(args)
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
