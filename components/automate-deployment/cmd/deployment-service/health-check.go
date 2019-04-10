// Copyright © 2017 Chef Software

package main

import (
	"fmt"
	"os"
	"time"

	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-deployment/pkg/client"
)

func init() {
	RootCmd.AddCommand(healthCheckCmd())
}

func healthCheckCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "health-check",
		Short: "checks server health",
		Long: `checks server health.

Environment:
  DEPLOYMENT_SERVICE_ADDRESS  set to a string like localhost:10160 to
                              configure the host and port
`,
		Run: runHealthCheck,
	}
}

func runHealthCheck(cmd *cobra.Command, args []string) {
	_, err := client.Connection(1 * time.Second)
	if err != nil {
		fmt.Printf("FAIL: %s\n", err.Error())
		// hab's health checks use the following exit codes:
		// * 0: ok
		// * 1: warning
		// * 2: critical
		// * 3: unknown
		// * any other code: failed health check with additional output taken from
		//   health_check stdout.
		// we'll prefer 4 to 2 since that includes more debug information.
		os.Exit(4)
	}
	os.Exit(0)

}
