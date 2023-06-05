package main

import (
	"os"

	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/infrastructure"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/spf13/cobra"
)

func init() {
	infrastructureCmd.AddCommand(nodeDeleteCmd)

	RootCmd.AddCommand(infrastructureCmd)
}

var infrastructureCmd = &cobra.Command{
	Use:               "infrastructure COMMAND",
	Short:             "Chef Automate infrastructure",
	Long:              "Commands for automation infrastructure management, for data related to chef-client runs and chef-server actions.",
	PersistentPreRunE: preInfrastructureCmd,
	Annotations: map[string]string{
		docs.Tag: docs.Automate,
	},
}

var nodeDeleteCmd = &cobra.Command{
	Use:   "node-delete [uuid]",
	Short: "Delete node by node uuid",
	Long:  "",
	RunE:  runDeleteNodeCmd,
	Args:  cobra.ExactArgs(1),
	Annotations: map[string]string{
		docs.Tag: docs.Automate,
	},
}

func runDeleteNodeCmd(cmd *cobra.Command, args []string) error {
	ifw, err := infrastructure.NewDeleteNode(writer)
	if err != nil {
		return err
	}
	return ifw.RunDeleteNode(args[0])
}

func preInfrastructureCmd(cmd *cobra.Command, args []string) error {
	err := commandPrePersistent(cmd)
	if err != nil {
		return status.Wrap(err, status.CommandExecutionError, "unable to set command parent settings")
	}
	if isA2HARBFileExist() {
		err = RunCmdOnSingleAutomateNode(cmd, args)
		if err != nil {
			return err
		}
		// NOTE: used os.exit as need to stop next lifecycle method to execute
		os.Exit(1)
	}
	return nil
}
