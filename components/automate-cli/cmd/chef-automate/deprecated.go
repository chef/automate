package main

import (
	"os"

	"github.com/spf13/cobra"
)

// A place to put all of our deprecated commands

func newAdminTokenCommand() *cobra.Command {
	return &cobra.Command{
		Use:    "admin-token",
		Short:  "[deprecated] Chef Automate IAM v1 admin token create",
		RunE:   runAdminTokenCmd,
		Hidden: true,
	}
}

func init() {
	adminCommand := newAdminTokenCommand()
	RootCmd.AddCommand(adminCommand)
}

func runAdminTokenCmd(cmd *cobra.Command, _ []string) error {
	writer.Failf("The admin-token command is deprecated. Use:\n\nchef-automate iam token create NAME --admin")
	os.Exit(1)
	return nil
}
