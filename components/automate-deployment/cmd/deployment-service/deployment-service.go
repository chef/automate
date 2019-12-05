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
	if err := os.Setenv("HAB_LICENSE", "accept-no-persist"); err != nil {
		fmt.Fprintf(os.Stderr, "could not set HAB_LICENSE=accept-no-persist: %s\n", err.Error())
		os.Exit(1)
	}

	if err := RootCmd.Execute(); err != nil {
		fmt.Fprintln(os.Stderr, err.Error()) // nolint: gas
		os.Exit(1)
	}
}
