package main

import (
	"context"
	"os"
	"sync"

	"github.com/spf13/cobra"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
)

const (
	RESTART_FRONTEND_COMMAND    = `sudo chef-automate restart-services`
	RESTART_BACKEND_COMMAND     = `sudo HAB_LICENSE=accept-no-persist systemctl restart hab-sup`
	DEFAULT_TIMEOUT_FOR_RESTART = 1200
	ERROR_ON_MANAGED_SERVICES   = "Restarting services on managed %s is not supported."
)

type RestartCmdFlags struct {
	automate   bool
	chefServer bool
	opensearch bool
	postgresql bool
	node       string
	timeout    int
}

func init() {
	var restartCmdFlags = RestartCmdFlags{}
	var restartServicesCmd = &cobra.Command{

		Use:   "restart-services",
		Short: "restart deployment services",
		Long:  "Restart services for a deployment",
		RunE:  runRestartServicesCmd(&restartCmdFlags),
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
	RootCmd.AddCommand(restartServicesCmd)
}

func runRestartServicesCmd(flags *RestartCmdFlags) func(cmd *cobra.Command, args []string) error {
	return func(cmd *cobra.Command, args []string) error {
		return runRestartServices(cmd, args, flags)
	}
}

func runRestartServices(cmd *cobra.Command, args []string, flags *RestartCmdFlags) error {
	if isA2HARBFileExist() {
		return runRestartFromBastion(flags, NewRestartFromBastionImpl())
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

type restartFromBastion interface {
	getAutomateHAInfraDetails() (*AutomateHAInfraDetails, error)
	isManagedServicesOn() bool
	executeRemoteExecutor(*NodeTypeAndCmd, SSHUtil, *cli.Writer) (map[string][]*CmdResult, error)
	printRestartCmdOutput(map[string][]*CmdResult, string, *sync.WaitGroup, *sync.Mutex, *cli.Writer)
}

type restartFromBastionImpl struct{}

func NewRestartFromBastionImpl() *restartFromBastionImpl {
	return &restartFromBastionImpl{}
}

func (rs *restartFromBastionImpl) getAutomateHAInfraDetails() (*AutomateHAInfraDetails, error) {
	return getAutomateHAInfraDetails()
}

func (st *restartFromBastionImpl) isManagedServicesOn() bool {
	return isManagedServicesOn()
}

func (rs *restartFromBastionImpl) executeRemoteExecutor(nodemap *NodeTypeAndCmd, sshUtil SSHUtil, writer *cli.Writer) (map[string][]*CmdResult, error) {
	remoteExecutor := NewRemoteCmdExecutor(nodemap, sshUtil, writer)
	cmdResult, err := remoteExecutor.Execute()
	return cmdResult, err
}

func (rs *restartFromBastionImpl) printRestartCmdOutput(cmdResult map[string][]*CmdResult, remoteService string, wg *sync.WaitGroup, mutex *sync.Mutex, writer *cli.Writer) {
	mutex.Lock()
	writer.Printf("=====================================================\n")
	for _, value := range cmdResult {
		for _, cmdResult := range value {
			printOutput(remoteService, *cmdResult, []string{}, writer)
		}
	}
	mutex.Unlock()
	wg.Done()
}

func runRestartFromBastion(flags *RestartCmdFlags, rs restartFromBastion) error {

	var wg = &sync.WaitGroup{}
	var mutex = &sync.Mutex{}
	infra, err := rs.getAutomateHAInfraDetails()
	if err != nil {
		return err
	}

	if !flags.automate && !flags.chefServer && !flags.opensearch && !flags.postgresql {
		if len(flags.node) != 0 {
			return status.Errorf(status.InvalidCommandArgsError, "Please provide service flag")
		}

		flags.automate = true
		flags.chefServer = true
		flags.opensearch = true
		flags.postgresql = true
	}
	errChan := make(chan error, 4)

	if flags.automate {
		go func(flags RestartCmdFlags, errChan chan<- error) {
			writer := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)
			sshUtil := NewSSHUtil(&SSHConfig{})
			nodeMap := constructNodeMapForAllNodeTypes(&flags, infra)
			cmdResult, err := rs.executeRemoteExecutor(nodeMap, sshUtil, writer)
			errChan <- err
			wg.Add(1)
			go rs.printRestartCmdOutput(cmdResult, AUTOMATE, wg, mutex, writer)
		}(*flags, errChan)
	} else {
		errChan <- nil
	}

	if flags.chefServer {
		go func(flags RestartCmdFlags, errChan chan<- error) {
			writer := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)
			sshUtil := NewSSHUtil(&SSHConfig{})
			flags.automate = false
			nodeMap := constructNodeMapForAllNodeTypes(&flags, infra)
			cmdResult, err := rs.executeRemoteExecutor(nodeMap, sshUtil, writer)
			errChan <- err
			wg.Add(1)
			go rs.printRestartCmdOutput(cmdResult, CHEF_SERVER, wg, mutex, writer)
		}(*flags, errChan)
	} else {
		errChan <- nil
	}

	if rs.isManagedServicesOn() {
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
		go func(flags RestartCmdFlags, errChan chan<- error) {
			writer := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)
			sshUtil := NewSSHUtil(&SSHConfig{})
			flags.automate = false
			flags.chefServer = false
			flags.opensearch = false
			nodeMap := constructNodeMapForAllNodeTypes(&flags, infra)
			cmdResult, err := rs.executeRemoteExecutor(nodeMap, sshUtil, writer)
			errChan <- err
			wg.Add(1)
			go rs.printRestartCmdOutput(cmdResult, POSTGRESQL, wg, mutex, writer)
		}(*flags, errChan)
	} else {
		errChan <- nil
	}

	if flags.opensearch {
		go func(flags RestartCmdFlags, errChan chan<- error) {
			writer := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)
			sshUtil := NewSSHUtil(&SSHConfig{})
			flags.automate = false
			flags.chefServer = false
			flags.postgresql = false
			nodeMap := constructNodeMapForAllNodeTypes(&flags, infra)
			cmdResult, err := rs.executeRemoteExecutor(nodeMap, sshUtil, writer)
			errChan <- err
			wg.Add(1)
			go rs.printRestartCmdOutput(cmdResult, OPENSEARCH, wg, mutex, writer)
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

func handleManagedServiceError(flags *RestartCmdFlags) error {

	if flags.postgresql && flags.opensearch {
		return nil
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
				SkipPrintOutput:          true,
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
