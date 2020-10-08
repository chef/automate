package main

import (
	"fmt"
	"os"

	"github.com/chef/automate/components/sample-data-service/cmd/sample-data-service/commands"
)

func main() {
	if err := commands.RootCmd.Execute(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
