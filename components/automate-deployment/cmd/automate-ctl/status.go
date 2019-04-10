// Copyright © 2018 Chef Software

package main

import (
	"fmt"
	"os"

	"github.com/chef/automate/components/automate-deployment/pkg/a1upgrade"

	"github.com/spf13/cobra"
)

func init() {
	RootCmd.AddCommand(automateCtlStatusCmd())
}

func automateCtlStatusCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "status",
		Short: "test double of `automate-ctl status`",
		Long: `automate-ctl status will exit 0 by default.
To test failure cases set the FAILURE environment to 102:

FAILURE=102 automate-ctl status

`,
		Run: automateCtlStatus,
	}
}

func automateCtlStatus(cmd *cobra.Command, args []string) {
	if a1upgrade.Failure(a1upgrade.PreflightA1Down) {
		fmt.Println("automate-ctl status FAILURE (simulated)")
		os.Exit(1)
	}
	fmt.Println("automate-ctl status SUCCESS (simulated)")
	os.Exit(0)

}
