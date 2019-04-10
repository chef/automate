// Copyright © 2017 Chef Software

package main

import (
	"context"

	"github.com/spf13/cobra"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
)

var stopCmd = &cobra.Command{
	Use:   "stop",
	Short: "Stop deployment",
	Long:  "Stop a running deployment of Automate.",
	RunE:  runStopCmd,
}

func runStopCmd(cmd *cobra.Command, args []string) error {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return err
	}

	if isDevMode() {
		_, err = connection.Stop(context.Background(), &api.StopRequest{})
		if err != nil {
			return status.Wrap(
				err,
				status.DeploymentServiceCallError,
				"Request to stop services failed",
			)
		}
	} else {
		// In the non studio case, it is important to ask systemd to perform the stop.
		// There is currently a bug in habitat where it's possible for hab-launch to
		// stay alive: https://github.com/habitat-sh/habitat/issues/5783
		// The deployment-service converger does its best to work around this issue,
		// but for extra safety, trigger the stop through systemctl.
		t := target.NewLocalTarget(true)
		if err := t.EnsureStopped(); err != nil {
			return status.Wrap(err, status.UnknownError, "Failed to stop Automate")
		}
	}

	writer.Title("Chef Automate Stopped")
	return nil
}

func init() {
	RootCmd.AddCommand(stopCmd)
}
