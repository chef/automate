package main

import (
	"github.com/spf13/cobra"
)

func init() {
	RootCmd.AddCommand(backupCmd())
}

func backupCmd() *cobra.Command {
	root := &cobra.Command{
		Use: "backup",
	}

	restore := &cobra.Command{
		Use:  "restore backupPath",
		RunE: runRestore,
	}

	root.AddCommand(restore)
	return root
}

func runRestore(cmd *cobra.Command, args []string) error {
	return nil
}
