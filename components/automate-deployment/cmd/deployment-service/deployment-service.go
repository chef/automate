package main

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
)

// RootCmd contains all the deployment service commands
var RootCmd = &cobra.Command{
	Use:   "deployment-service [COMMAND]",
	Short: "Chef Automate Deployment Service",
}

func main() {
	if err := RootCmd.Execute(); err != nil {
		fmt.Fprintln(os.Stderr, err.Error()) // nolint: gas
		os.Exit(1)
	}
}
