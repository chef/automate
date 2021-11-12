// Copyright © 2017 Chef Software

package main

import (
	"github.com/spf13/cobra"
)

func init() {
	infoCmd.SetUsageTemplate(infoHelpDocs)
	RootCmd.AddCommand(infoCmd)
}

var infoCmd = &cobra.Command{
	Use:   "info",
	Short: "Info about Automate HA",
	Long:  "Info for Automate HA cluster",
	Annotations: map[string]string{
		NoCheckVersionAnnotation: NoCheckVersionAnnotation,
	},
	RunE: runInfoConfigCmd,
}

func runInfoConfigCmd(cmd *cobra.Command, args []string) error {
	return executeAutomateClusterCtlCommand("info", args, infoHelpDocs)
}
