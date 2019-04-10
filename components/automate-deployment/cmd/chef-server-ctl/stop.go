// Copyright © 2018 Chef Software

package main

import (
	"fmt"
	"os"

	"github.com/chef/automate/components/automate-deployment/pkg/a1stub"
	"github.com/chef/automate/components/automate-deployment/pkg/a1upgrade"

	"github.com/spf13/cobra"
)

func init() {
	RootCmd.AddCommand(chefServerCtlStopCmd())
}

func chefServerCtlStopCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "stop",
		Short: "test double of `chef-server-ctl stop`",
		Long: `chef-server-ctl stop will exit 0 by default.
To test failure cases set the FAILURE environment to 601:

FAILURE=601 chef-server-ctl stop

`,
		Run: chefServerCtlStop,
	}
}

func chefServerCtlStop(cmd *cobra.Command, args []string) {
	// The automate-ctl double will make a network call here to shutdown the stub
	// a1 server; we don't have a stub chef server so we don't need that part here.
	if a1upgrade.Failure(a1upgrade.ChefServerShutdownFail) {
		fmt.Println("chef-server-ctl stop FAILURE (simulated)")
		os.Exit(1)
	}

	if len(args) > 0 && args[0] == "nginx" {
		f, err := os.OpenFile(a1stub.ChefServerNginxStoppedSentinel, os.O_RDWR|os.O_CREATE, 0755)
		if err != nil {
			fmt.Println("chef-server-ctl stop nginx FAILURE (could not write stopped sentinel)")
			os.Exit(1)
		}
		f.Close() // nolint: errcheck
	}

	fmt.Println("chef-server-ctl stop SUCCESS (simulated)")
	os.Exit(0)

}
