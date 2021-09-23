// Copyright © 2017 Chef Software

package main

import (
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
	if len(args) == 0 {
		writer.Print("please refer \n" + secretsHelpDocs)
		return nil
	}
	return executeAutomateClusterCtlCommand("secrets", args, secretsHelpDocs)
	//return executeSecretCommand(args)
}

/* func executeSecretCommand(args []string) error {
	if len(args) == 0 {
		writer.Print("please refer \n" + secretsHelpDocs)
		return nil
	}
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
		writer.Printf(stderr.String())
		return status.Wrap(err, status.CommandExecutionError, "please refer \n"+secretsHelpDocs)
	}
	writer.Print(out.String())
	writer.Printf("A2HA new secret set. %d, exiting\n", c.Process.Pid)
	return err
} */
