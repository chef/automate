package main

import (
	"errors"

	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
)

var testHAHelpDocs = `
Usage:
    chef-automate test
		run smoke tests manually.
`
var testCommandFlags = struct {
	full bool
}{}

var testCmd = &cobra.Command{
	Use:   "test",
	Short: "Run Automate HA smoke tests",
	Long:  "Run smoke test for Automate HA services.",
	Args:  cobra.RangeArgs(0, 1),
	RunE:  runTestCmd,
	Annotations: map[string]string{
		docs.Compatibility: docs.CompatiblewithHA,
	},
}

func runTestCmd(cmd *cobra.Command, args []string) error {
	if isA2HARBFileExist() {
		if testCommandFlags.full {
			args = append(args, "--full")
		}
		return executeAutomateClusterCtlCommandAsync("test", args, testHAHelpDocs, false)
	} else {
		return status.Wrap(errors.New(AUTOMATE_HA_INVALID_BASTION), status.ConfigError, testHAHelpDocs)
	}
}

func init() {
	testCmd.PersistentFlags().BoolVar(
		&testCommandFlags.full,
		"full",
		false,
		"Automate ha cluster test full")
	RootCmd.AddCommand(testCmd)
}
