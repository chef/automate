package main

import (
	"errors"

	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-cli/pkg/status"
)

var testHAHelpDocs = `
Usage:
    chef-automate test
		run smoke tests manually.
`

func newTestCmd() *cobra.Command {
	var testCmd = &cobra.Command{
		Use:   "test",
		Short: "Run automate HA smoke tests",
		Long:  "Run smoke test for automate HA services.",
		Args:  cobra.RangeArgs(0, 1),
		RunE:  runTestCmd,
	}

	return testCmd
}

func runTestCmd(cmd *cobra.Command, args []string) error {
	if isA2HARBFileExist() {
		return executeAutomateClusterCtlCommand("test", args, testHAHelpDocs)
	} else {
		return status.Wrap(errors.New("Test command only work with HA mode of automate"), status.ConfigError, testHAHelpDocs)
	}

}

func init() {
	RootCmd.AddCommand(newTestCmd())
}
