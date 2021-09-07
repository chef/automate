// Copyright © 2017 Chef Software

package main

import (
	"bytes"
	"os"
	"os/exec"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/spf13/cobra"
)

func init() {
	SshCmd.SetUsageTemplate(SshCommandHelpDocs)
	RootCmd.AddCommand(SshCmd)
}

var SshCmd = &cobra.Command{
	Use:   "ssh",
	Short: "set Ssh env for automate HA.",
	Long:  "set up automate ha clusterSsh",
	Annotations: map[string]string{
		NoCheckVersionAnnotation: NoCheckVersionAnnotation,
	},
	Args: cobra.RangeArgs(0, 2),
	RunE: runSshCmd,
}

func runSshCmd(cmd *cobra.Command, args []string) error {
	return executeSshCommand(args)
}

func executeSshCommand(args []string) error {
	if len(args) == 0 {
		writer.Print("please refer \n" + SshCommandHelpDocs)
		return nil
	}
	writer.Printf("Login into cluster components \n")
	args = append([]string{"ssh"}, args...)
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
		return status.Wrap(err, status.CommandExecutionError, "please refer \n"+SshCommandHelpDocs)
	}
	writer.Print(out.String())
	writer.Printf(" %d, exiting\n", c.Process.Pid)
	return err
}