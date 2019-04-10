// Copyright © 2017 Chef Software

package main

import (
	"context"
	"io"

	"github.com/spf13/cobra"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
)

var systemLogsCmd = &cobra.Command{
	Use:   "system-logs",
	Short: "Tail Chef Automate logs",
	Long:  "Start streaming logs from the Chef Automate server. Ctrl + c to stop.",
	RunE:  runSystemLogsCmd,
}

func runSystemLogsCmd(cmd *cobra.Command, args []string) error {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return err
	}
	stream, err := connection.SystemLogs(context.Background(), &api.SystemLogsRequest{})
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to get system logs failed",
		)
	}
	for {
		logLine, err := stream.Recv()
		if err == io.EOF {
			break
		}
		if err != nil {
			return status.Wrap(
				err,
				status.DeploymentServiceCallError,
				"Request to stream system logs failed",
			)
		}
		writer.Print(logLine.Line)
	}

	return nil
}

func init() {
	RootCmd.AddCommand(systemLogsCmd)
}
