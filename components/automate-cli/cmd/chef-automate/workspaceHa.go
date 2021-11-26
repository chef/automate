// Copyright © 2017 Chef Software

package main

import (
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func init() {
	workspaceCmd.SetUsageTemplate(workspaceCommandHelpDocs)
	RootCmd.AddCommand(workspaceCmd)
}

var workspaceCmd = &cobra.Command{
	Use:   "workspace",
	Short: "Set workspace env for Automate HA.",
	Long:  "Set up Automate HA cluster workspace.",
	Annotations: map[string]string{
		NoCheckVersionAnnotation: NoCheckVersionAnnotation,
	},
	RunE: runWorkspaceCmd,
}

func runWorkspaceCmd(cmd *cobra.Command, args []string) error {
	if isA2HARBFileExist() {
		if len(args) == 0 {
			writer.Print("please refer \n" + workspaceCommandHelpDocs)
			return nil
		}
		return executeAutomateClusterCtlCommand("workspace", args, workspaceCommandHelpDocs)
	}
	return status.Wrap(errors.New(AUTOMATE_HA_INVALID_BASTION), status.InvalidCommandArgsError, workspaceCommandHelpDocs)
}
