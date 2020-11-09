package commands

import (
	"github.com/spf13/cobra"
)

// RootCmd is our application root command.
var RootCmd = &cobra.Command{
	Use:   "sample-data-service",
	Short: "Chef Automate sample data service",
}

func init() {
	RootCmd.AddCommand(serveCmd)
}
