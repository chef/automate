// Copyright © 2017 Chef Software

package main

import (
	"context"
	"fmt"
	"os"
	"sync"
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

type statusCmdFlags struct {
	waitForHealthy      bool
	waitTimeout         int64
	waitRefreshInterval int64
	automate            bool
	chefServer          bool
	postgresql          bool
	opensearch          bool
	node                string
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
}
type FeStatus []FeStatusValue
type BeStatus []BeStatusValue

const (
	STATUS_ERROR_ON_SELF_MANAGED = "Showing the status for externally configured %s is not supported."
)

func newStatusCmd() *cobra.Command {

	statusCmdFlag := statusCmdFlags{}

	var statusCmd = &cobra.Command{
		Use:   "status",
		Short: "Retrieve Chef Automate status",
		Long:  "Retrieve Chef Automate status. Includes status of Automate services.",
		RunE:  runStatusCmdGetter(&statusCmdFlag), //and call the function here which returs function
	}

	statusCmd.PersistentFlags().BoolVarP(
		&statusCmdFlag.waitForHealthy, "wait-for-healthy", "w", false,
		"Wait until the status response is healthy or the timeout is reached",
	)
	statusCmd.PersistentFlags().SetAnnotation("wait-for-healthy", docs.Compatibility, []string{docs.CompatiblewithStandalone})
	statusCmd.PersistentFlags().Int64VarP(
		&statusCmdFlag.waitTimeout, "wait-timeout", "t", 600,
		"How many seconds to wait for the status to be healthy before returning an error",
	)
	statusCmd.PersistentFlags().SetAnnotation("wait-timeout", docs.Compatibility, []string{docs.CompatiblewithStandalone})
	statusCmd.PersistentFlags().Int64VarP(
		&statusCmdFlag.waitRefreshInterval, "wait-refresh-interval", "r", 2,
		"How many seconds to wait between polling for status updates",
	)
	statusCmd.PersistentFlags().SetAnnotation("wait-refresh-interval", docs.Compatibility, []string{docs.CompatiblewithStandalone})

	statusCmd.PersistentFlags().BoolVar(&statusCmdFlag.automate, "automate", false, "Shows status from Automate node(HA)")
	statusCmd.PersistentFlags().SetAnnotation("automate", docs.Compatibility, []string{docs.CompatiblewithHA})
	statusCmd.PersistentFlags().BoolVar(&statusCmdFlag.chefServer, "chef_server", false, "Shows status from Chef-server node(HA)")
	statusCmd.PersistentFlags().SetAnnotation("chef_server", docs.Compatibility, []string{docs.CompatiblewithHA})
	statusCmd.PersistentFlags().BoolVar(&statusCmdFlag.postgresql, "postgresql", false, "Shows status from PostgresQL node")
	statusCmd.PersistentFlags().SetAnnotation("postgresql", docs.Compatibility, []string{docs.CompatiblewithHA})
	statusCmd.PersistentFlags().BoolVar(&statusCmdFlag.opensearch, "opensearch", false, "Shows status from OpenSearch node")
	statusCmd.PersistentFlags().SetAnnotation("opensearch", docs.Compatibility, []string{docs.CompatiblewithHA})
	statusCmd.PersistentFlags().BoolVar(&statusCmdFlag.automate, "a2", false, "Shows status from Automate node(HA)[DUPLICATE]")
	statusCmd.PersistentFlags().SetAnnotation("a2", docs.Compatibility, []string{docs.CompatiblewithHA})
	statusCmd.PersistentFlags().BoolVar(&statusCmdFlag.chefServer, "cs", false, "Shows status from Chef-server node(HA)[DUPLICATE]")
	statusCmd.PersistentFlags().SetAnnotation("cs", docs.Compatibility, []string{docs.CompatiblewithHA})
	statusCmd.PersistentFlags().BoolVar(&statusCmdFlag.postgresql, "pg", false, "Shows status from PostgresQL node[DUPLICATE]")
	statusCmd.PersistentFlags().SetAnnotation("pg", docs.Compatibility, []string{docs.CompatiblewithHA})
	statusCmd.PersistentFlags().BoolVar(&statusCmdFlag.opensearch, "os", false, "Shows status from OpenSearch node[DUPLICATE]")
	statusCmd.PersistentFlags().SetAnnotation("os", docs.Compatibility, []string{docs.CompatiblewithHA})
	statusCmd.PersistentFlags().StringVar(&statusCmdFlag.node, "node", "", "Pass this flag to check status of perticular node node(HA)")
	statusCmd.PersistentFlags().SetAnnotation("node", docs.Compatibility, []string{docs.CompatiblewithHA})

	statusCmd.AddCommand(newStatusSummaryCmd())
	return statusCmd
}

func runStatusCmdGetter(flags *statusCmdFlags) func(cmd *cobra.Command, args []string) error {
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
	statusSummary := NewStatusSummary(infra, FeStatus{}, BeStatus{}, 10, time.Second, statusSummaryCmdFlags, sshUtil)
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

func runStatusCmdFlowExecutor(cmd *cobra.Command, args []string, flags *statusCmdFlags) error {
	if isA2HARBFileExist() {
		return runStatusFromBastion(flags)
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

func runStatusFromBastion(flags *statusCmdFlags) error {

	var wg = &sync.WaitGroup{}
	var mutex = &sync.Mutex{}

	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		return err
	}

	if !flags.automate && !flags.chefServer && !flags.opensearch && !flags.postgresql {
		flags.automate = true
		flags.chefServer = true
		flags.opensearch = true
		flags.postgresql = true
	}
	errChan := make(chan error, 4)

	if flags.automate {
		go func(flags statusCmdFlags, errChan chan<- error) {
			writer := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)
			sshUtil := NewSSHUtil(&SSHConfig{})
			nodeMap := constructNodeMapForStatus(&flags, infra)
			cmdResult, err := executeRemoteExecutor(nodeMap, sshUtil, writer)
			errChan <- err
			wg.Add(1)
			go printStatusOutput(cmdResult, AUTOMATE, infra.Outputs.AutomatePrivateIps.Value, wg, mutex, writer)
		}(*flags, errChan)
	} else {
		errChan <- nil
	}

	if flags.chefServer {
		go func(flags statusCmdFlags, errChan chan<- error) {
			writer := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)
			sshUtil := NewSSHUtil(&SSHConfig{})
			flags.automate = false
			nodeMap := constructNodeMapForStatus(&flags, infra)
			cmdResult, err := executeRemoteExecutor(nodeMap, sshUtil, writer)
			errChan <- err
			wg.Add(1)
			go printStatusOutput(cmdResult, CHEF_SERVER, infra.Outputs.ChefServerPrivateIps.Value, wg, mutex, writer)
		}(*flags, errChan)
	} else {
		errChan <- nil
	}

	if isManagedServicesOn() {
		errChan <- nil
		errChan <- nil
		for i := 0; i < 4; i++ {
			errVal := <-errChan
			if errVal != nil {
				return errVal
			}
		}
		wg.Wait()
		return handleManagedServiceError(flags)
	}

	if flags.postgresql {
		go func(flags statusCmdFlags, errChan chan<- error) {
			writer := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)
			sshUtil := NewSSHUtil(&SSHConfig{})
			flags.automate = false
			flags.chefServer = false
			flags.opensearch = false
			nodeMap := constructNodeMapForStatus(&flags, infra)
			cmdResult, err := executeRemoteExecutor(nodeMap, sshUtil, writer)
			errChan <- err
			wg.Add(1)
			go printStatusOutput(cmdResult, POSTGRESQL, infra.Outputs.PostgresqlPrivateIps.Value, wg, mutex, writer)
		}(*flags, errChan)
	} else {
		errChan <- nil
	}

	if flags.opensearch {
		go func(flags statusCmdFlags, errChan chan<- error) {
			writer := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)
			sshUtil := NewSSHUtil(&SSHConfig{})
			flags.automate = false
			flags.chefServer = false
			flags.postgresql = false
			nodeMap := constructNodeMapForStatus(&flags, infra)
			cmdResult, err := executeRemoteExecutor(nodeMap, sshUtil, writer)
			errChan <- err
			wg.Add(1)
			go printStatusOutput(cmdResult, OPENSEARCH, infra.Outputs.OpensearchPrivateIps.Value, wg, mutex, writer)
		}(*flags, errChan)
	} else {
		errChan <- nil
	}

	for i := 0; i < 4; i++ {
		errVal := <-errChan
		if errVal != nil {
			return errVal
		}
	}
	wg.Wait()
	return nil
}

func executeRemoteExecutor(nodemap *NodeTypeAndCmd, sshUtil SSHUtil, writer *cli.Writer) (map[string][]*CmdResult, error) {
	remoteExecutor := NewRemoteCmdExecutor(nodemap, sshUtil, writer)
	cmdResult, err := remoteExecutor.Execute()
	return cmdResult, err
}

func printStatusOutput(cmdResult map[string][]*CmdResult, remoteService string, nodeIps []string, wg *sync.WaitGroup, mutex *sync.Mutex, writer *cli.Writer) {
	mutex.Lock()
	writer.Printf("=====================================================%s=======================================================\n", "Status on "+remoteService)
	for _, value := range cmdResult {
		for _, cmdResult := range value {
			printOutput(remoteService, *cmdResult, []string{}, writer)
		}
	}
	mutex.Unlock()
	wg.Done()
}

func handleManagedServiceError(flags *statusCmdFlags) error {

	if flags.postgresql && flags.opensearch {
		return nil
	}

	if flags.postgresql {
		return status.Errorf(status.InvalidCommandArgsError, STATUS_ERROR_ON_SELF_MANAGED, POSTGRESQL)
	}

	if flags.opensearch {
		return status.Errorf(status.InvalidCommandArgsError, STATUS_ERROR_ON_SELF_MANAGED, OPENSEARCH)
	}

	return status.Errorf(status.InvalidCommandArgsError, "Please provide supported flag")
}

func constructNodeMapForStatus(flags *statusCmdFlags, infra *AutomateHAInfraDetails) *NodeTypeAndCmd {
	commandFrontEnd := buildFrontEndStatusCmd(flags)
	commandBackEnd := "sudo HAB_LICENSE=accept-no-persist hab svc status"
	nodeMap := &NodeTypeAndCmd{
		Frontend: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      commandFrontEnd,
				ErrorCheckEnableInOutput: true,
				WaitTimeout:              int(flags.waitTimeout),
				Single:                   false,
				NodeType:                 false,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true,
			},
		},
		Automate: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      commandFrontEnd,
				Single:                   false,
				ErrorCheckEnableInOutput: true,
				WaitTimeout:              int(flags.waitTimeout),
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
				Single:                   false,
				ErrorCheckEnableInOutput: true,
				NodeIps:                  []string{flags.node},
				NodeType:                 flags.chefServer,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true,
			},
		},
		Postgresql: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      commandBackEnd,
				NodeIps:                  []string{flags.node},
				Single:                   false,
				ErrorCheckEnableInOutput: true,
				NodeType:                 flags.postgresql,
				WaitTimeout:              600,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true,
			},
		},
		Opensearch: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      commandBackEnd,
				NodeIps:                  []string{flags.node},
				Single:                   false,
				ErrorCheckEnableInOutput: true,
				NodeType:                 flags.opensearch,
				WaitTimeout:              600,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true,
			},
		},
		Infra: infra,
	}
	return nodeMap
}

func buildFrontEndStatusCmd(flags *statusCmdFlags) string {
	command := "sudo chef-automate status"

	if flags.waitForHealthy {
		command += " -w"
	}

	command += " -r" + " " + fmt.Sprintf("%v", flags.waitRefreshInterval)
	command += " -t" + " " + fmt.Sprintf("%v", flags.waitTimeout)

	return command
}

func init() {
	cmd := newStatusCmd()
	RootCmd.AddCommand(cmd)
}
