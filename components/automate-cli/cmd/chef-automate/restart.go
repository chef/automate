package main

import (
	"context"
	"os"
	"strings"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/logger"
)

const (
	RESTART_FRONTEND_COMMAND    = `sudo chef-automate restart-services`
	RESTART_BACKEND_COMMAND     = `sudo systemctl restart hab-sup`
	DEFAULT_TIMEOUT_FOR_RESTART = 1200
	ERROR_ON_MANAGED_SERVICES   = "Restart for externally configured %s is not supported."
	CMD_FAILED_MSG              = "Command failed on %s node : %s with error:\n %s\n"
)

type RestartCmdFlags struct {
	automate   bool
	chefServer bool
	opensearch bool
	postgresql bool
	node       string
	timeout    int
}

type restartCmdResult struct {
	cmdResult map[string][]*CmdResult
	writer    *cli.Writer
	nodeType  string
	err       error
}

func init() {
	var restartCmdFlags = RestartCmdFlags{}
	var restartServicesCmd = &cobra.Command{

		Use:   "restart-services",
		Short: "restart deployment services",
		Long:  "Restart services for a deployment",
		//PersistentPreRunE: WarnLicenseStatusForExpiry,
		RunE: runRestartServicesCmd(&restartCmdFlags),
		Annotations: map[string]string{
			docs.Tag: docs.BastionHost,
		},
	}
	restartServicesCmd.PersistentFlags().BoolVarP(&restartCmdFlags.automate, "automate", "a", false, "restart chef automate service on automate nodes")
	restartServicesCmd.PersistentFlags().BoolVar(&restartCmdFlags.automate, "a2", false, "restart chef automate service on automate nodes[DUPLICATE]")
	restartServicesCmd.PersistentFlags().SetAnnotation("automate", docs.Compatibility, []string{docs.CompatiblewithHA})
	restartServicesCmd.PersistentFlags().BoolVarP(&restartCmdFlags.chefServer, "chef_server", "c", false, "restart chef automate service on chef-server nodes")
	restartServicesCmd.PersistentFlags().BoolVar(&restartCmdFlags.chefServer, "cs", false, "restart chef automate service on chef-server nodes[DUPLICATE]")
	restartServicesCmd.PersistentFlags().SetAnnotation("chef_server", docs.Compatibility, []string{docs.CompatiblewithHA})
	restartServicesCmd.PersistentFlags().BoolVarP(&restartCmdFlags.opensearch, "opensearch", "o", false, "restart hab-sup service on opensearch nodes")
	restartServicesCmd.PersistentFlags().BoolVar(&restartCmdFlags.opensearch, "os", false, "restart hab-sup service on opensearch nodes[DUPLICATE]")
	restartServicesCmd.PersistentFlags().SetAnnotation("opensearch", docs.Compatibility, []string{docs.CompatiblewithHA})
	restartServicesCmd.PersistentFlags().BoolVarP(&restartCmdFlags.postgresql, "postgresql", "p", false, "restart hab-sup service on postgresql nodes")
	restartServicesCmd.PersistentFlags().BoolVar(&restartCmdFlags.postgresql, "pg", false, "restart hab-sup service on postgresql nodes[DUPLICATE]")
	restartServicesCmd.PersistentFlags().SetAnnotation("pg", docs.Compatibility, []string{docs.CompatiblewithHA})
	restartServicesCmd.PersistentFlags().StringVar(&restartCmdFlags.node, "node", "", "Node Ip address")
	restartServicesCmd.PersistentFlags().SetAnnotation("node", docs.Compatibility, []string{docs.CompatiblewithHA})
	restartServicesCmd.PersistentFlags().IntVar(&restartCmdFlags.timeout, "wait-timeout", DEFAULT_TIMEOUT_FOR_RESTART, "This flag sets the operation timeout duration (in seconds) for each individual node during the restart services")
	restartServicesCmd.PersistentFlags().SetAnnotation("wait-timeout", docs.Compatibility, []string{docs.CompatiblewithHA})
	RootCmd.AddCommand(restartServicesCmd)
}

func runRestartServicesCmd(flags *RestartCmdFlags) func(cmd *cobra.Command, args []string) error {
	return func(cmd *cobra.Command, args []string) error {
		return runRestartServices(cmd, args, flags)
	}
}

func runRestartServices(cmd *cobra.Command, args []string, flags *RestartCmdFlags) error {
	if isA2HARBFileExist() {
		nodeOpUtils := &NodeUtilsImpl{}
		remoteExcecutor := NewRemoteCmdExecutorWithoutNodeMap(&SSHUtilImpl{}, cli.NewWriter(os.Stdout, os.Stderr, os.Stdin))
		return runRestartFromBastion(flags, remoteExcecutor, nodeOpUtils, printRestartOutput)
	}
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return err
	}
	resp, err := connection.RestartServices(context.Background(), &api.RestartServicesRequest{})
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to restart services failed",
		)
	}
	writer.Title("Services are restarting")
	deploymentID := &api.DeploymentID{}
	h := &client.CLIEventWriter{Writer: writer}
	err = connection.StreamDeployEvents(resp.TaskId, deploymentID, h)
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to stream restart events failed",
		)
	}

	return nil
}

func runRestartFromBastion(flags *RestartCmdFlags, rs RemoteCmdExecutor, nu NodeOpUtils, printRestartOutput func(map[string][]*CmdResult, string, *cli.Writer)) error {
	infra, _, err := nu.getHaInfraDetails()
	if err != nil {
		return err
	}
	if flags.timeout < DEFAULT_TIMEOUT_FOR_RESTART {
		return errors.Errorf("The operation timeout duration for each individual node during the restart should be set to a value greater than %v seconds.", DEFAULT_TIMEOUT_FOR_RESTART)
	}
	if !flags.automate && !flags.chefServer && !flags.opensearch && !flags.postgresql {
		if len(flags.node) != 0 {
			return status.Errorf(status.InvalidCommandArgsError, "Please provide service flag")
		}
		flags.automate = true
		flags.chefServer = true
		if !nu.isManagedServicesOn() {
			flags.opensearch = true
			flags.postgresql = true
		}
	}
	restartCmdResults := make(chan restartCmdResult, 4)
	runRestartCmdForFrontEnd(infra, flags, rs, restartCmdResults)

	if nu.isManagedServicesOn() {
		restartCmdResults <- restartCmdResult{}
		restartCmdResults <- restartCmdResult{}
		err = getChannelValue(restartCmdResults, printRestartOutput)
		if err != nil {
			return err
		}
		return handleManagedServices(flags)
	}

	runRestartCmdForBackend(infra, flags, rs, restartCmdResults)

	return getChannelValue(restartCmdResults, printRestartOutput)
}

func runRestartCmdForFrontEnd(infra *AutomateHAInfraDetails, flags *RestartCmdFlags, rs RemoteCmdExecutor, restartCmdResults chan restartCmdResult) {
	if flags.automate {
		restartOnGivenNode(flags, AUTOMATE, infra, rs, restartCmdResults)
	} else {
		restartCmdResults <- restartCmdResult{}
	}

	if flags.chefServer {
		flags.automate = false
		restartOnGivenNode(flags, CHEF_SERVER, infra, rs, restartCmdResults)
	} else {
		restartCmdResults <- restartCmdResult{}
	}
}

func runRestartCmdForBackend(infra *AutomateHAInfraDetails, flags *RestartCmdFlags, rs RemoteCmdExecutor, restartCmdResults chan restartCmdResult) {
	flags.automate = false
	flags.chefServer = false
	if flags.postgresql {
		restartOnGivenNode(flags, POSTGRESQL, infra, rs, restartCmdResults)
		reloadPgConfig(restartCmdResults)

	} else {
		restartCmdResults <- restartCmdResult{}
	}
	if flags.opensearch {
		flags.postgresql = false
		restartOnGivenNode(flags, OPENSEARCH, infra, rs, restartCmdResults)
	} else {
		restartCmdResults <- restartCmdResult{}
	}
}

func reloadPgConfig(restartCmdResults chan restartCmdResult) {
	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		restartCmdResults <- restartCmdResult{
			err: err,
		}
	}
	level := "info"
	if globalOpts.debug {
		level = "debug"
	}
	log, err := logger.NewLogger("text", level)
	if err != nil {
		restartCmdResults <- restartCmdResult{
			err: err,
		}
	}
	fileUtils := &fileutils.FileSystemUtils{}
	nodeOpUtils := &NodeUtilsImpl{}
	conf := SSHConfig{
		sshUser:    infra.Outputs.SSHUser.Value,
		sshPort:    infra.Outputs.SSHPort.Value,
		sshKeyFile: infra.Outputs.SSHKeyFile.Value,
		timeout:    DEFAULT_TIMEOUT,
	}
	err = nodeOpUtils.postPGCertRotate(infra.Outputs.PostgresqlPrivateIps.Value, conf, fileUtils, log)
	if err != nil {
		restartCmdResults <- restartCmdResult{
			err: err,
		}
	}
}

func restartOnGivenNode(flags *RestartCmdFlags, nodeType string, infra *AutomateHAInfraDetails, rs RemoteCmdExecutor, restartCmdResults chan restartCmdResult) {
	go func(flags RestartCmdFlags, restartCmdResults chan<- restartCmdResult) {
		writer := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)
		rs.SetWriter(writer)
		nodeMap := constructNodeMapForAllNodeTypes(&flags, infra)
		cmdResult, err := rs.ExecuteWithNodeMap(nodeMap)
		restartCmdResults <- restartCmdResult{
			cmdResult: cmdResult,
			writer:    writer,
			nodeType:  nodeType,
			err:       err,
		}
	}(*flags, restartCmdResults)
}

func printRestartOutput(cmdResult map[string][]*CmdResult, remoteService string, writer *cli.Writer) {
	writer.Printf("\n============================================================================================================\n")

	for _, value := range cmdResult {
		for _, cmdResult := range value {
			if cmdResult.Error != nil {
				printRestartErrorOutput(cmdResult, remoteService, writer)
			} else {
				writer.Printf("Output for Host IP %s : \n%s", cmdResult.HostIP, cmdResult.Output+"\n")
				writer.Success("Command is executed on " + remoteService + " node : " + cmdResult.HostIP + "\n")
			}
			writer.BufferWriter().Flush()
		}
	}
}

func printRestartErrorOutput(cmdResult *CmdResult, remoteService string, writer *cli.Writer) {
	isOutputError := false
	if strings.Contains(cmdResult.Output, "DeploymentServiceUnreachableError") {
		isOutputError = true
		writer.Failf(CMD_FAILED_MSG, remoteService, cmdResult.HostIP, cmdResult.Output)
	}
	if !isOutputError {
		writer.Failf(CMD_FAILED_MSG, remoteService, cmdResult.HostIP, cmdResult.Error.Error())
	}
}

func getChannelValue(restartCmdResults chan restartCmdResult, printRestartOutput func(map[string][]*CmdResult, string, *cli.Writer)) error {
	for i := 0; i < 4; i++ {
		cmdResult := <-restartCmdResults
		if cmdResult.err != nil {
			return cmdResult.err
		} else {
			if cmdResult.writer != nil {
				printRestartOutput(cmdResult.cmdResult, cmdResult.nodeType, cmdResult.writer)
			}
		}
	}
	return nil
}

func handleManagedServices(flags *RestartCmdFlags) error {
	if flags.postgresql && flags.opensearch {
		return status.Errorf(status.InvalidCommandArgsError, ERROR_ON_MANAGED_SERVICES, "postgresql and opensearch")
	}
	if flags.postgresql {
		return status.Errorf(status.InvalidCommandArgsError, ERROR_ON_MANAGED_SERVICES, POSTGRESQL)
	}
	if flags.opensearch {
		return status.Errorf(status.InvalidCommandArgsError, ERROR_ON_MANAGED_SERVICES, OPENSEARCH)
	}
	return nil
}

func constructNodeMapForAllNodeTypes(flags *RestartCmdFlags, infra *AutomateHAInfraDetails) *NodeTypeAndCmd {
	nodeMap := &NodeTypeAndCmd{
		Frontend: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      RESTART_FRONTEND_COMMAND,
				Single:                   false,
				NodeType:                 false,
				HideSSHConnectionMessage: true,
			},
		},
		Automate: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      RESTART_FRONTEND_COMMAND,
				Single:                   false,
				ErrorCheckEnableInOutput: true,
				WaitTimeout:              int(flags.timeout),
				NodeIps:                  []string{flags.node},
				NodeType:                 flags.automate,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true,
			},
		},
		ChefServer: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      RESTART_FRONTEND_COMMAND,
				WaitTimeout:              int(flags.timeout),
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
				Cmd:                      RESTART_BACKEND_COMMAND,
				WaitTimeout:              int(flags.timeout),
				NodeIps:                  []string{flags.node},
				Single:                   false,
				ErrorCheckEnableInOutput: true,
				NodeType:                 flags.postgresql,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true,
			},
		},
		Opensearch: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      RESTART_BACKEND_COMMAND,
				WaitTimeout:              int(flags.timeout),
				NodeIps:                  []string{flags.node},
				Single:                   false,
				ErrorCheckEnableInOutput: true,
				NodeType:                 flags.opensearch,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true,
			},
		},
		Infra: infra,
	}
	return nodeMap
}
