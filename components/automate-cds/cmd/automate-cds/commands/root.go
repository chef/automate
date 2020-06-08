package commands

import (
	"github.com/spf13/cobra"
)

// RootCmd is our application root command.
var RootCmd = &cobra.Command{
	Use:   "automate-cds",
	Short: "Chef Automate content delivery service proxy",
}

func init() {
	RootCmd.AddCommand(serveCmd)
}
