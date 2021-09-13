package main

import (
	"bytes"
	"errors"
	"os"
	"os/exec"

	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-cli/pkg/status"
)

func newTestCmd() *cobra.Command {
	var testCmd = &cobra.Command{
		Use:   "provision-infra",
		Short: "Provison automate HA infra.",
		Long:  "Provison automate HA infra for automate HA deployment.",
		Args:  cobra.RangeArgs(0, 1),
		RunE:  runTestCmd,
	}

	return testCmd
}

func runTestCmd(cmd *cobra.Command, args []string) error {
	if isA2HADeployment() {
		writer.Printf("prvisioning infra for automate HA \n")
		args = append([]string{"provision"}, args...)
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
			return status.Wrap(err, status.CommandExecutionError, "")
		}
		writer.Print(out.String())
		return err
	} else {
		return status.Wrap(errors.New("provision-infra only works for HA mode of automate"), status.ConfigError, "")
	}

}

func init() {
	RootCmd.AddCommand(newTestCmd())
}
