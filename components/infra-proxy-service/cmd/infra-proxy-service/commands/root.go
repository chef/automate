package commands

import (
	"github.com/spf13/cobra"
)

// RootCmd is our application root command.
var RootCmd = &cobra.Command{
	Use:   "infra-proxy-service",
	Short: "Chef Automate infra proxy service",
}

func init() {
	RootCmd.AddCommand(serveCmd)
}
