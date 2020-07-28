// Copyright © 2017 Chef Software

package main

import (
	"context"
	"time"

	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
)

var statusCmdFlags = struct {
	waitForHealthy      bool
	waitTimeout         int64
	waitRefreshInterval int64
}{}

func newStatusCmd() *cobra.Command {
	var statusCmd = &cobra.Command{
		Use:   "status",
		Short: "Retrieve Chef Automate status",
		Long:  "Retrieve Chef Automate status. Includes status of Automate services.",
		RunE:  runStatusCmd,
	}

	statusCmd.PersistentFlags().BoolVarP(
		&statusCmdFlags.waitForHealthy, "wait-for-healthy", "w", false,
		"Wait until the status response is healthy or the timeout is reached",
	)
	statusCmd.PersistentFlags().Int64VarP(
		&statusCmdFlags.waitTimeout, "wait-timeout", "t", 600,
		"How many seconds to wait for the status to be healthy before returning an error",
	)
	statusCmd.PersistentFlags().Int64VarP(
		&statusCmdFlags.waitRefreshInterval, "wait-refresh-interval", "r", 2,
		"How many seconds to wait between polling for status updates",
	)

	return statusCmd
}

type statusResult struct {
	Services []api.FormattedServiceStatus `json:"services"`
}

var (
	statusErrUndeployed = status.New(status.UnhealthyStatusError, "No services have been deployed")
	statusErrUnhealthy  = status.New(status.UnhealthyStatusError, "One or more services are unhealthy")
)

func runStatusCmd(cmd *cobra.Command, args []string) error {
	writeStatus := func(res *api.StatusResponse) {
		writer.Titlef(
			"Status from deployment with channel [%s] and type [%s]",
			res.DeploymentConfig.V1.Svc.Channel.GetValue(),
			res.DeploymentConfig.V1.Svc.DeploymentType.GetValue(),
		)
		writer.Title(res.ServiceStatus.FormatStatus())
		status.GlobalResult = statusResult{
			Services: res.ServiceStatus.FormattedServices(),
		}
	}

	getStatus := func() (*api.StatusResponse, error) {
		connection, err := client.Connection(client.DefaultClientTimeout)
		if err != nil {
			return nil, err
		}

		res, err := connection.Status(context.Background(), &api.StatusRequest{})
		if err != nil {
			return nil, status.Wrap(
				err,
				status.DeploymentServiceCallError,
				"Request to obtain Chef Automate status information failed",
			)
		}

		return res, nil
	}

	handleBadStatus := func(res *api.StatusResponse, err error) error {
		if err != nil {
			return err
		}

		writeStatus(res)

		if len(res.ServiceStatus.Services) == 0 {
			return statusErrUndeployed
		}

		return statusErrUnhealthy
	}

	startTime := time.Now()
	res, err := getStatus()
	if err == nil {
		if res.ServiceStatus.AllHealthy() {
			writeStatus(res)
			return nil
		}
	}

	if !statusCmdFlags.waitForHealthy {
		return handleBadStatus(res, err)
	}

	timeout := startTime.Add(time.Second * time.Duration(statusCmdFlags.waitTimeout))
	refresh := time.NewTicker(time.Second * time.Duration(statusCmdFlags.waitRefreshInterval)).C

	// Listen to the refresh channel and query for the status. If the timeout
	// elapses before a healthy status has been returned then write the status
	// if possible and return an error.
	for {
		select {
		case <-refresh:
			logrus.Debug("Refreshing status")
			res, err := getStatus()
			if err == nil {
				if res.ServiceStatus.AllHealthy() {
					writeStatus(res)
					return nil
				}
			}

			if time.Now().After(timeout) {
				logrus.Debug("Timeout elapsed")
				return handleBadStatus(res, err)
			}
		}
	}
}

func init() {
	RootCmd.AddCommand(newStatusCmd())
}
