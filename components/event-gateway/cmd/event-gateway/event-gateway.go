package main

import (
	"fmt"
	"os"

	"github.com/chef/automate/components/event-gateway/cmd/event-gateway/commands"
)

func main() {
	if err := commands.RootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
