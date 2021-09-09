package main

import (
	"bytes"
	"os"
	"os/exec"

	"github.com/chef/automate/components/automate-cli/pkg/status"
)

func executeHAGatherLogsA2HA(args []string) error {
	writer.Printf("gathering A2HA logs \n")
	args = append([]string{"gather-logs"}, args...)
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
		return status.Wrap(err, status.CommandExecutionError, "please refer \n"+gatherLogsHelpDoc)
	}
	writer.Print(out.String())
	return err
}
