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
	infoCmd.SetUsageTemplate(infoHelpDocs)
	RootCmd.AddCommand(infoCmd)
}

var infoCmd = &cobra.Command{
	Use:   "info",
	Short: "InFo about automate HA",
	Long:  "Info for automate HA cluster",
	Annotations: map[string]string{
		NoCheckVersionAnnotation: NoCheckVersionAnnotation,
	},
	//Args: cobra.RangeArgs(0, 2),
	RunE: runInfoConfigCmd,
}

func runInfoConfigCmd(cmd *cobra.Command,args []string) error {
	return executeInfoCommand()
}

func executeInfoCommand() error {
	writer.Printf("Automate HA info \n")
	//args = append([]string{"info"})
	c := exec.Command("automate-cluster-ctl","info")
	c.Dir = "/hab/a2_deploy_workspace"
	c.Stdin = os.Stdin
	var out bytes.Buffer
	var stderr bytes.Buffer
	c.Stdout = &out
	c.Stderr = &stderr
	err := c.Run()
	if err != nil {
		writer.Printf(stderr.String())
		return status.Wrap(err, status.CommandExecutionError, "please refer \n"+infoHelpDocs)
	}
	writer.Print(out.String())
	//writer.Printf("A2HA new secret set. %d, exiting\n", c.Process.Pid)
	return err
}
