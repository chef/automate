package main

import (
	"fmt"
	"os"

	"github.com/chef/automate/components/automate-cds/cmd/automate-cds/commands"
)

func main() {
	if err := commands.RootCmd.Execute(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
