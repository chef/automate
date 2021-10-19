// Copyright © 2017 Chef Software

package main

import (
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/spf13/cobra"
)

func init() {
	secretsCmd.SetUsageTemplate(secretsHelpDocs)
	RootCmd.AddCommand(secretsCmd)
}

var secretsCmd = &cobra.Command{
	Use:   "secrets",
	Short: "set secrets to automate HA",
	Long:  "set secrets for automate sudo password and admin password in HA mode.",
	Annotations: map[string]string{
		NoCheckVersionAnnotation: NoCheckVersionAnnotation,
	},
	Args: cobra.RangeArgs(0, 2),
	RunE: runSecretsConfigCmd,
}

func runSecretsConfigCmd(cmd *cobra.Command, args []string) error {
	return executeSecretsCommand(args)
}

func executeSecretsCommand(args []string) error {
	if isA2HARBFileExist() {
		if len(args) == 0 {
			writer.Print("please refer \n" + secretsHelpDocs)
			return nil
		}
		return executeAutomateClusterCtlCommand("secrets", args, secretsHelpDocs)
	}
	return status.New(status.InvalidCommandArgsError, secretsHelpDocs)
}
