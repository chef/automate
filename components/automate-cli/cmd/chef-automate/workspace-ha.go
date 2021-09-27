// Copyright © 2017 Chef Software

package main

import (
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/spf13/cobra"
)

func init() {
	workspaceCmd.SetUsageTemplate(workspaceCommandHelpDocs)
	RootCmd.AddCommand(workspaceCmd)
}

var workspaceCmd = &cobra.Command{
	Use:   "workspace",
	Short: "set workspace env for automate HA.",
	Long:  "set up automate ha cluster workspace.",
	Annotations: map[string]string{
		NoCheckVersionAnnotation: NoCheckVersionAnnotation,
	},
	Args: cobra.RangeArgs(0, 2),
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
	return status.New(status.InvalidCommandArgsError, workspaceCommandHelpDocs)
}
