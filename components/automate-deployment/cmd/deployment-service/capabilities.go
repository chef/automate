package main

import (
	"fmt"

	"github.com/spf13/cobra"
)

func init() {
	RootCmd.AddCommand(capabilitiesCmd())
}

var Capabilities = []string{
	"bootstrap",
	"bootstrap-bundle",
}

func capabilitiesCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "capabilities",
		Short: "list the supported capabilities the deployment-service",
		Run:   runCapabilities,
	}
}

func runCapabilities(cmd *cobra.Command, args []string) {
	for _, cap := range Capabilities {
		fmt.Println(cap)
	}
}
