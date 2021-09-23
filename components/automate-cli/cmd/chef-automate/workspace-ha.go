// Copyright © 2017 Chef Software

package main

import (
	"github.com/spf13/cobra"
)

func init() {
	workspaceCmd.SetUsageTemplate(workspaceCommandHelpDocs)
	RootCmd.AddCommand(workspaceCmd)
}

var workspaceCmd = &cobra.Command{
	Use:   "workspace",
	Short: "set workspace env for automate HA.",
	Long:  "set up automate ha cluster workspace.",
	Annotations: map[string]string{
		NoCheckVersionAnnotation: NoCheckVersionAnnotation,
	},
	Args: cobra.RangeArgs(0, 2),
	RunE: runWorkspaceCmd,
}

func runWorkspaceCmd(cmd *cobra.Command, args []string) error {
	if len(args) == 0 {
		writer.Print("please refer \n" + workspaceCommandHelpDocs)
		return nil
	}
	return executeAutomateClusterCtlCommand("workspace", args, workspaceCommandHelpDocs)
	//return executeWorkspaceCommand(args)
}

/* func executeWorkspaceCommand(args []string) error {
	if len(args) == 0 {
		writer.Print("please refer \n" + workspaceCommandHelpDocs)
		return nil
	}
	writer.Printf("setting automate HA cluster workspace \n")
	args = append([]string{"workspace"}, args...)
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
		return status.Wrap(err, status.CommandExecutionError, "please refer \n"+workspaceCommandHelpDocs)
	}
	writer.Print(out.String())
	writer.Printf("workspace cluster setup done. %d, exiting\n", c.Process.Pid)
	return err
} */
