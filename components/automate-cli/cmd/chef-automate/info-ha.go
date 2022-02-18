// Copyright © 2017 Chef Software

package main

import (
	"errors"

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
	if isA2HARBFileExist() {
		return executeAutomateClusterCtlCommand("info", args, infoHelpDocs)
	}
	return errors.New(AUTOMATE_HA_INVALID_BASTION)
}
