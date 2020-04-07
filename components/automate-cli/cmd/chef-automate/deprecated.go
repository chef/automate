package main

import (
	"os"

	"github.com/spf13/cobra"
)

// A place to put all of our deprecated commands

func newAdminTokenCommand() *cobra.Command {
	return &cobra.Command{
		Use:    "admin-token NAME",
		Short:  "[deprecated] Chef Automate IAM v1 admin token create",
		RunE:   runAdminTokenCmd,
		Args:   cobra.ExactArgs(1),
		Hidden: true,
	}
}

func init() {
	adminCommand := newAdminTokenCommand()
	RootCmd.AddCommand(adminCommand)
}

func runAdminTokenCmd(cmd *cobra.Command, args []string) error {
	name := args[0]

	writer.Failf("The admin-token command is deprecated. "+
		"Use, run:\n\nchef-automate iam token create %s --admin", name)
	os.Exit(1)
	return nil
}
