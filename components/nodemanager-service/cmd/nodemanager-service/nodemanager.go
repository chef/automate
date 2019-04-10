package main

import (
	"fmt"
	"os"

	"github.com/chef/automate/components/nodemanager-service/cmd/nodemanager-service/cmd"
)

// This is the entrypoint for the CLI
func main() {
	if err := cmd.RootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
