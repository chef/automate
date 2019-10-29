package main

import (
	"fmt"
	"os"

	"github.com/chef/automate/components/infra-proxy/cmd/infra-proxy/commands"
)

func main() {
	if err := commands.RootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
