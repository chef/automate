// Copyright © 2017 Chef Software

package main

import (
	"context"
	"fmt"
	"time"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

var statusCmdFlags = struct {
	waitForHealthy      bool
	waitTimeout         int64
	waitRefreshInterval int64
}{}

type StatusSummaryCmdFlags struct {
	node         string
	isAutomate   bool
	isChefServer bool
	isOpenSearch bool
	isPostgresql bool
}

type FeStatusValue struct {
	serviceName string
	ipAddress   string
	status      string
	Opensearch  string
}

type BeStatusValue struct {
	serviceName string
	ipAddress   string
	health      string
	process     string
	upTime      string
	role        string
}
type FeStatus []FeStatusValue
type BeStatus []BeStatusValue

var statusCmd = &cobra.Command{
	Use:   "status",
	Short: "Retrieve Chef Automate status",
	Long:  "Retrieve Chef Automate status. Includes status of Automate services.",
	RunE:  runStatusCmd,
}

func newStatusCmd() *cobra.Command {

	statusCmd.PersistentFlags().BoolVarP(
		&statusCmdFlags.waitForHealthy, "wait-for-healthy", "w", false,
		"Wait until the status response is healthy or the timeout is reached",
	)
	statusCmd.PersistentFlags().SetAnnotation("wait-for-healthy", docs.Compatibility, []string{docs.CompatiblewithStandalone})
	statusCmd.PersistentFlags().Int64VarP(
		&statusCmdFlags.waitTimeout, "wait-timeout", "t", 600,
		"How many seconds to wait for the status to be healthy before returning an error",
	)
	statusCmd.PersistentFlags().SetAnnotation("wait-timeout", docs.Compatibility, []string{docs.CompatiblewithStandalone})
	statusCmd.PersistentFlags().Int64VarP(
		&statusCmdFlags.waitRefreshInterval, "wait-refresh-interval", "r", 2,
		"How many seconds to wait between polling for status updates",
	)
	statusCmd.PersistentFlags().SetAnnotation("wait-refresh-interval", docs.Compatibility, []string{docs.CompatiblewithStandalone})

	return statusCmd
}
func newStatusSummaryCmd() *cobra.Command {
	var statusSummaryCmdFlags = StatusSummaryCmdFlags{}
	var statusSummaryCmd = &cobra.Command{
		Use:   "summary",
		Short: "Retrieve Chef Automate status-summary",
		Long:  "Retrieve Chef Automate status node summary for HA deployment",
		RunE:  runStatusSummaryCmdFunc(&statusSummaryCmdFlags),
	}
	statusSummaryCmd.PersistentFlags().BoolVarP(&statusSummaryCmdFlags.isAutomate, "automate", "a", false, "Get only automate Status")
	statusSummaryCmd.PersistentFlags().BoolVarP(&statusSummaryCmdFlags.isChefServer, "chef-server", "c", false, "Get only chef server Status")
	statusSummaryCmd.PersistentFlags().BoolVarP(&statusSummaryCmdFlags.isOpenSearch, "opensearch", "o", false, "Get only opensearch Status")
	statusSummaryCmd.PersistentFlags().BoolVarP(&statusSummaryCmdFlags.isPostgresql, "postgresql", "p", false, "Get only postgresql Status")
	statusSummaryCmd.PersistentFlags().StringVar(&statusSummaryCmdFlags.node, "node", "", "Node Ip address")

	return statusSummaryCmd
}

type statusResult struct {
	Services []api.FormattedServiceStatus `json:"services"`
}

var (
	statusErrUndeployed = status.New(status.UnhealthyStatusError, "No services have been deployed")
	statusErrUnhealthy  = status.New(status.UnhealthyStatusError, "One or more services are unhealthy")
)

func getStatus() (*api.StatusResponse, error) {
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

func runStatusSummaryCmdFunc(statusSummaryCmdFlags *StatusSummaryCmdFlags) func(cmd *cobra.Command, args []string) error {
	return func(cmd *cobra.Command, args []string) error {
		return executeStatusSummary(cmd, args, statusSummaryCmdFlags)
	}
}

func executeStatusSummary(cmd *cobra.Command, args []string, statusSummaryCmdFlags *StatusSummaryCmdFlags) error {

	if !isA2HARBFileExist() {
		return errors.New("This command only works on HA deployment")
	}

	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		return err
	}
	statusSummary := NewStatusSummary(infra, FeStatus{}, BeStatus{}, 10, time.Second, statusSummaryCmdFlags)
	sshUtil := statusSummary.(*Summary).getSSHConfig()

	err = statusSummary.Prepare(sshUtil)
	if err != nil {
		return err
	}

	// Display Status summary
	fe := statusSummary.ShowFEStatus()
	fmt.Println(fe)
	if !isManagedServicesOn() {
		be := statusSummary.ShowBEStatus()
		fmt.Println(be)
	}
	return nil
}

func runStatusCmd(cmd *cobra.Command, args []string) error {
	if isA2HARBFileExist() {
		return executeAutomateClusterCtlCommand("status", args, statusHAHelpDocs)
	}
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
	statusCmd.AddCommand(newStatusSummaryCmd())
	RootCmd.AddCommand(newStatusCmd())
}
