package main

import (
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
)

var uninstallLong = `Uninstall Chef Automate, deleting all data and configuration`

var uninstallFlags = client.UninstallOpts{}

var uninstallCmd = &cobra.Command{
	Use:   "uninstall",
	Short: "Uninstall Chef Automate",
	Long:  uninstallLong,
	Annotations: map[string]string{
		NoCheckVersionAnnotation: NoCheckVersionAnnotation,
	},
	RunE: runUninstallCmd,
}

func runUninstallCmd(cmd *cobra.Command, args []string) error {
	err := client.Destroy(writer, uninstallFlags)
	if err != nil {
		return status.Annotate(err, status.UninstallError)
	}
	return nil
}

func init() {
	uninstallCmd.PersistentFlags().BoolVar(
		&uninstallFlags.Yes,
		"yes",
		false,
		"Uninstall Chef Automate and destroy data without confirmation prompt")
	uninstallCmd.PersistentFlags().BoolVar(
		&uninstallFlags.PreservePkgCache,
		"preserve-package-cache",
		false,
		"Preserve Habitat package cache (useful for faster reinstall)")
	RootCmd.AddCommand(uninstallCmd)
}
