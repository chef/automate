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

		/* writer.Printf("executing smoke test for automate HA \n")
		args = append([]string{"test"}, args...)
		c := exec.Command("automate-cluster-ctl", args...)
		c.Dir = "/hab/a2_deploy_workspace"
		c.Stdin = os.Stdin
		var out bytes.Buffer
		var stderr bytes.Buffer
		c.Stdout = &out
		c.Stderr = &stderr
		err := c.Run()
		if err != nil {
			writer.Printf(stderr.String())
			return status.Wrap(err, status.CommandExecutionError, testHAHelpDocs)
		}
		writer.Print(out.String())
		return err */
	} else {
		return status.Wrap(errors.New("Test command only work with HA mode of automate"), status.ConfigError, testHAHelpDocs)
	}

}

func init() {
	RootCmd.AddCommand(newTestCmd())
}
