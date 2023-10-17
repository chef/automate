// Copyright © 2017 Chef Software

package main

import (
	"context"
	"fmt"
	"os"
	"strings"
	"time"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

type StatusCmdFlags struct {
	waitForHealthy      bool
	waitTimeout         int64
	waitRefreshInterval int64
	automate            bool
	chefServer          bool
	postgresql          bool
	opensearch          bool
	node                string
	acceptHabLicense    bool
}
type StatusCmdResult struct {
	cmdResult map[string][]*CmdResult
	writer    *cli.Writer
	nodeType  string
	err       error
}
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
	lag         string
}

type FeStatus []FeStatusValue
type BeStatus []BeStatusValue

const (
	STATUS_ERROR_ON_SELF_MANAGED          = "Showing the status for externally configured %s is not supported."
	CMD_FAIL_MSG                          = "Command failed on %s node : %s with error:\n %s\n"
	BACKEND_STATUS_CMD                    = "sudo HAB_LICENSE=accept-no-persist hab svc status"
	ACCEPTED_LICENENSE_BACKEND_STATUS_CMD = "sudo echo yes | hab svc status"
	FRONTEND_STATUS_CMD                   = "sudo chef-automate status"
)

func newStatusCmd() *cobra.Command {

	statusCmdFlag := StatusCmdFlags{}

	var statusCmd = &cobra.Command{
		Use:   "status",
		Short: "Retrieve Chef Automate status",
		Long:  "Retrieve Chef Automate status. Includes status of Automate services.",
		RunE:  runStatusCmdGetter(&statusCmdFlag),
		Annotations: map[string]string{
			docs.Tag: docs.BastionHost,
		},
	}

	statusCmd.Flags().BoolVarP(
		&statusCmdFlag.waitForHealthy, "wait-for-healthy", "w", false,
		"Wait until the status response is healthy or the timeout is reached",
	)

	statusCmd.Flags().Int64VarP(
		&statusCmdFlag.waitTimeout, "wait-timeout", "t", 600,
		"How many seconds to wait for the status to be healthy before returning an error",
	)

	statusCmd.Flags().Int64VarP(
		&statusCmdFlag.waitRefreshInterval, "wait-refresh-interval", "r", 2,
		"How many seconds to wait between polling for status updates",
	)

	statusCmd.Flags().BoolVarP(&statusCmdFlag.automate, "automate", "a", false, "Shows status from Automate nodes")
	statusCmd.Flags().SetAnnotation("automate", docs.Compatibility, []string{docs.CompatiblewithHA})
	statusCmd.Flags().BoolVarP(&statusCmdFlag.chefServer, "chef_server", "c", false, "Shows status from Chef-server nodes")
	statusCmd.Flags().SetAnnotation("chef_server", docs.Compatibility, []string{docs.CompatiblewithHA})
	statusCmd.Flags().BoolVarP(&statusCmdFlag.postgresql, "postgresql", "p", false, "Shows status from PostgresQL nodes")
	statusCmd.Flags().SetAnnotation("postgresql", docs.Compatibility, []string{docs.CompatiblewithHA})
	statusCmd.Flags().BoolVarP(&statusCmdFlag.opensearch, "opensearch", "o", false, "Shows status from OpenSearch nodes")
	statusCmd.Flags().SetAnnotation("opensearch", docs.Compatibility, []string{docs.CompatiblewithHA})
	statusCmd.Flags().BoolVar(&statusCmdFlag.automate, "a2", false, "Shows status from Automate nodes[DUPLICATE]")
	statusCmd.Flags().SetAnnotation("a2", docs.Compatibility, []string{docs.CompatiblewithHA})
	statusCmd.Flags().BoolVar(&statusCmdFlag.chefServer, "cs", false, "Shows status from Chef-server nodes[DUPLICATE]")
	statusCmd.Flags().SetAnnotation("cs", docs.Compatibility, []string{docs.CompatiblewithHA})
	statusCmd.Flags().BoolVar(&statusCmdFlag.postgresql, "pg", false, "Shows status from PostgresQL nodes[DUPLICATE]")
	statusCmd.Flags().SetAnnotation("pg", docs.Compatibility, []string{docs.CompatiblewithHA})
	statusCmd.Flags().BoolVar(&statusCmdFlag.opensearch, "os", false, "Shows status from OpenSearch nodes[DUPLICATE]")
	statusCmd.Flags().SetAnnotation("os", docs.Compatibility, []string{docs.CompatiblewithHA})
	statusCmd.Flags().StringVar(&statusCmdFlag.node, "node", "", "Pass this flag to check status of perticular node in the cluster")
	statusCmd.Flags().SetAnnotation("node", docs.Compatibility, []string{docs.CompatiblewithHA})
	statusCmd.Flags().StringVar(&statusCmdFlag.node, "--accept-hab-license", "", "Pass this flag to accept hab license for PostgresQL/OpenSearch nodes")

	statusCmd.AddCommand(newStatusSummaryCmd())
	return statusCmd
}

func runStatusCmdGetter(flags *StatusCmdFlags) func(cmd *cobra.Command, args []string) error {
	return func(cmd *cobra.Command, args []string) error {
		return runStatusCmdFlowExecutor(cmd, args, flags)
	}
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
	statusSummaryCmd.PersistentFlags().SetAnnotation("automate", docs.Compatibility, []string{docs.CompatiblewithHA})

	statusSummaryCmd.PersistentFlags().BoolVarP(&statusSummaryCmdFlags.isChefServer, "chef-server", "c", false, "Get only chef server Status")
	statusSummaryCmd.PersistentFlags().SetAnnotation("chef-server", docs.Compatibility, []string{docs.CompatiblewithHA})

	statusSummaryCmd.PersistentFlags().BoolVarP(&statusSummaryCmdFlags.isOpenSearch, "opensearch", "o", false, "Get only opensearch Status")
	statusSummaryCmd.PersistentFlags().SetAnnotation("opensearch", docs.Compatibility, []string{docs.CompatiblewithHA})

	statusSummaryCmd.PersistentFlags().BoolVarP(&statusSummaryCmdFlags.isPostgresql, "postgresql", "p", false, "Get only postgresql Status")
	statusSummaryCmd.PersistentFlags().SetAnnotation("postgresql", docs.Compatibility, []string{docs.CompatiblewithHA})

	statusSummaryCmd.PersistentFlags().StringVar(&statusSummaryCmdFlags.node, "node", "", "Node Ip address")
	statusSummaryCmd.PersistentFlags().SetAnnotation("node", docs.Compatibility, []string{docs.CompatiblewithHA})

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
	sshUtil := NewSSHUtil(&SSHConfig{})
	remoteCmdExecutor := NewRemoteCmdExecutorWithoutNodeMap(sshUtil, writer)
	statusSummary := NewStatusSummary(infra, FeStatus{}, BeStatus{}, 10, time.Second, statusSummaryCmdFlags, remoteCmdExecutor)
	err = statusSummary.Prepare()
	if err != nil {
		return err
	}

	// Display Status summary
	fe := statusSummary.ShowFEStatus()
	fmt.Println(fe)
	if !isManagedServicesOn() {
		be := statusSummary.ShowBEStatus()
		fmt.Println(be)
	} else {
		writer.Warn("-o and -p flag is not supported for deployment with managed services\n")
	}
	writer.BufferWriter().Flush()

	return nil
}

func runStatusCmdFlowExecutor(cmd *cobra.Command, args []string, flags *StatusCmdFlags) error {
	if isA2HARBFileExist() {
		nodeOpUtils := &NodeUtilsImpl{}
		remoteExecutor := NewRemoteCmdExecutorWithoutNodeMap(&SSHUtilImpl{}, cli.NewWriter(os.Stdout, os.Stderr, os.Stdin))
		return runStatusFromBastion(flags, nodeOpUtils, remoteExecutor, printStatusOutput)
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

	if !flags.waitForHealthy {
		return handleBadStatus(res, err)
	}

	timeout := startTime.Add(time.Second * time.Duration(flags.waitTimeout))
	refresh := time.NewTicker(time.Second * time.Duration(flags.waitRefreshInterval)).C

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

func runStatusFromBastion(flags *StatusCmdFlags, nodeOpUtils NodeOpUtils, remoteExe RemoteCmdExecutor, printStatusOutput func(map[string][]*CmdResult, string, *cli.Writer)) error {

	infra, _, err := nodeOpUtils.getHaInfraDetails()
	if err != nil {
		return err
	}

	if !flags.automate && !flags.chefServer && !flags.opensearch && !flags.postgresql {
		if len(flags.node) != 0 {
			return status.Errorf(status.InvalidCommandArgsError, "Please provide service flag")
		}

		flags.automate = true
		flags.chefServer = true

		if !nodeOpUtils.isManagedServicesOn() {
			flags.opensearch = true
			flags.postgresql = true
		}
	}

	statusCmdResults := make(chan StatusCmdResult, 4)

	runStatusCmdForFrontEnd(infra, flags, statusCmdResults, remoteExe)

	if nodeOpUtils.isManagedServicesOn() {
		statusCmdResults <- StatusCmdResult{}
		statusCmdResults <- StatusCmdResult{}
		err = getValueFromChannel(statusCmdResults, printStatusOutput)
		if err != nil {
			return err
		}
		return handleManagedServiceErrorForStatusCmd(flags)
	}

	runStatusCmdForBackend(infra, flags, statusCmdResults, remoteExe)

	return getValueFromChannel(statusCmdResults, printStatusOutput)
}

func runStatusCmdForFrontEnd(infra *AutomateHAInfraDetails, flags *StatusCmdFlags, statusCmdResults chan StatusCmdResult, remoteExe RemoteCmdExecutor) {

	if flags.automate {
		go nodeStatus(*flags, AUTOMATE, infra, statusCmdResults, remoteExe)
	} else {
		statusCmdResults <- StatusCmdResult{}
	}

	if flags.chefServer {
		flags.automate = false
		go nodeStatus(*flags, CHEF_SERVER, infra, statusCmdResults, remoteExe)
	} else {
		statusCmdResults <- StatusCmdResult{}
	}
}

func runStatusCmdForBackend(infra *AutomateHAInfraDetails, flags *StatusCmdFlags, statusCmdResults chan StatusCmdResult, remoteExe RemoteCmdExecutor) {

	if flags.postgresql {
		flags.automate = false
		flags.chefServer = false
		go nodeStatus(*flags, POSTGRESQL, infra, statusCmdResults, remoteExe)
	} else {
		statusCmdResults <- StatusCmdResult{}
	}

	if flags.opensearch {
		flags.automate = false
		flags.chefServer = false
		flags.postgresql = false
		go nodeStatus(*flags, OPENSEARCH, infra, statusCmdResults, remoteExe)
	} else {
		statusCmdResults <- StatusCmdResult{}
	}
}

func nodeStatus(flags StatusCmdFlags, nodeType string, infra *AutomateHAInfraDetails, statusCmdResults chan StatusCmdResult, remoteExe RemoteCmdExecutor) {
	writer := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)
	remoteExe.SetWriter(writer)
	nodeMap := constructNodeMapForStatus(&flags, infra)
	cmdResult, err := remoteExe.ExecuteWithNodeMap(nodeMap)
	statusCmdResults <- StatusCmdResult{
		cmdResult: cmdResult,
		writer:    writer,
		nodeType:  nodeType,
		err:       err,
	}
}

func printStatusOutput(cmdResult map[string][]*CmdResult, remoteService string, writer *cli.Writer) {
	writer.Printf("\n=====================================================%s=======================================================\n", "Status on "+remoteService)

	for _, value := range cmdResult {
		for _, cmdResult := range value {
			if cmdResult.Error != nil {
				printStatusErrorOutput(cmdResult, remoteService, writer)
			} else {
				writer.Printf("Output for Host IP %s : \n%s", cmdResult.HostIP, cmdResult.Output+"\n")
				writer.Success("Command is executed on " + remoteService + " node : " + cmdResult.HostIP + "\n")
			}
			writer.BufferWriter().Flush()
		}
	}
}

func printStatusErrorOutput(cmdResult *CmdResult, remoteService string, writer *cli.Writer) {
	isOutputError := false
	if strings.Contains(cmdResult.Output, "UnhealthyStatusError") {
		isOutputError = true
		writer.Failf("Output for Host IP %s : \n%s", cmdResult.HostIP, cmdResult.Output+"\n")
		writer.Success("Command is executed on " + remoteService + " node : " + cmdResult.HostIP + "\n")
	}

	if strings.Contains(cmdResult.Output, "DeploymentServiceUnreachableError") {
		isOutputError = true
		writer.Failf(CMD_FAIL_MSG, remoteService, cmdResult.HostIP, cmdResult.Output)
	}

	if !isOutputError {
		writer.Failf(CMD_FAIL_MSG, remoteService, cmdResult.HostIP, cmdResult.Error.Error())
	}
}

func getValueFromChannel(statusCmdResults chan StatusCmdResult, printStatusOutput func(map[string][]*CmdResult, string, *cli.Writer)) error {
	for i := 0; i < 4; i++ {
		cmdResult := <-statusCmdResults
		if cmdResult.err != nil {
			return cmdResult.err
		} else {
			if cmdResult.writer != nil {
				printStatusOutput(cmdResult.cmdResult, cmdResult.nodeType, cmdResult.writer)
			}
		}
	}
	return nil
}

func handleManagedServiceErrorForStatusCmd(flags *StatusCmdFlags) error {

	if flags.postgresql && flags.opensearch {
		return status.Errorf(status.InvalidCommandArgsError, STATUS_ERROR_ON_SELF_MANAGED, POSTGRESQL+" and "+OPENSEARCH)
	}

	if flags.postgresql {
		return status.Errorf(status.InvalidCommandArgsError, STATUS_ERROR_ON_SELF_MANAGED, POSTGRESQL)
	}

	if flags.opensearch {
		return status.Errorf(status.InvalidCommandArgsError, STATUS_ERROR_ON_SELF_MANAGED, OPENSEARCH)
	}

	return nil
}

func constructNodeMapForStatus(flags *StatusCmdFlags, infra *AutomateHAInfraDetails) *NodeTypeAndCmd {
	commandFrontEnd := buildFrontEndStatusCmd(flags)
	nodeMap := &NodeTypeAndCmd{
		Frontend: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      commandFrontEnd,
				WaitTimeout:              int(flags.waitTimeout),
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true,
			},
		},
		Automate: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      commandFrontEnd,
				WaitTimeout:              int(flags.waitTimeout),
				ErrorCheckEnableInOutput: true,
				NodeIps:                  []string{flags.node},
				NodeType:                 flags.automate,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true,
			},
		},
		ChefServer: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      commandFrontEnd,
				WaitTimeout:              int(flags.waitTimeout),
				ErrorCheckEnableInOutput: true,
				NodeIps:                  []string{flags.node},
				NodeType:                 flags.chefServer,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true,
			},
		},
		Postgresql: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      BACKEND_STATUS_CMD,
				NodeIps:                  []string{flags.node},
				ErrorCheckEnableInOutput: true,
				NodeType:                 flags.postgresql,
				WaitTimeout:              int(flags.waitTimeout),
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true,
			},
		},
		Opensearch: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      BACKEND_STATUS_CMD,
				NodeIps:                  []string{flags.node},
				ErrorCheckEnableInOutput: true,
				NodeType:                 flags.opensearch,
				WaitTimeout:              int(flags.waitTimeout),
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true,
			},
		},
		Infra: infra,
	}
	return nodeMap
}

func buildFrontEndStatusCmd(flags *StatusCmdFlags) string {
	command := FRONTEND_STATUS_CMD

	if flags.waitForHealthy {
		command += " -w"
		command += " -t" + " " + fmt.Sprintf("%v", flags.waitTimeout)
	}

	command += " -r" + " " + fmt.Sprintf("%v", flags.waitRefreshInterval)

	return command
}

func init() {
	cmd := newStatusCmd()
	RootCmd.AddCommand(cmd)
}
