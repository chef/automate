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
	return executeSecretCommand(args)
}

func executeSecretCommand(args []string) error {
	writer.Printf("setting A2HA secrets \n")
	args = append([]string{"secrets"}, args...)
	c := exec.Command("automate-cluster-ctl", args...)
	c.Dir = "/hab/a2_deploy_workspace"
	c.Stdin = os.Stdin
	var out bytes.Buffer
	var stderr bytes.Buffer
	c.Stdout = &out
	c.Stderr = &stderr
	err := c.Run()
	if err != nil {
		writer.Printf(err.Error())
		return status.Wrap(err, status.CommandExecutionError, stderr.String())
	}
	writer.Print(string(out.String()))
	writer.Printf("A2HA new secret set. %d, exiting\n", c.Process.Pid)
	return err
}
