// Copyright © 2018 Chef Software

package main

import (
	"crypto/tls"
	"fmt"
	"net/http"
	"os"

	"github.com/chef/automate/components/automate-deployment/pkg/a1upgrade"

	"github.com/spf13/cobra"
)

func init() {
	RootCmd.AddCommand(automateCtlstopCmd())
}

// automateCtl/stopCmd represents the automateCtl/stop command
func automateCtlstopCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "stop",
		Short: "test double of `automate-ctl stop`",
		Long: `automate-ctl stop will exit 0 by default.
To test failure cases set the FAILURE environment to 600:

FAILURE=600 automate-ctl stop

`,
		Run: automateCtlStop,
	}
}

func automateCtlStop(cmd *cobra.Command, args []string) {
	// the a1 stub server only supports localhost:443
	tlsConfig := &tls.Config{InsecureSkipVerify: true}
	tr := &http.Transport{TLSClientConfig: tlsConfig}
	client := &http.Client{Transport: tr}
	_, err := client.Get("https://localhost/th-ctl/stop")
	if err != nil {
		fmt.Printf("automate-ctl stop TEST HARNESS ERROR %s\n", err.Error())
		os.Exit(10)
	}
	if a1upgrade.Failure(a1upgrade.A1ShutdownFail) {
		fmt.Println("automate-ctl stop FAILURE (simulated)")
		os.Exit(1)
	}
	fmt.Println("automate-ctl stop SUCCESS (simulated)")
	os.Exit(0)

}
