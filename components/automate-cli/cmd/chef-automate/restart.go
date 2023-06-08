package main

import (
	"context"
	"os"
	"sync"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
)

const (
	RESTART_FRONTEND_COMMAND     = `sudo chef-automate restart-services`
	RESTART_BACKEND_COMMAND      = `sudo systemctl restart hab-sup`
	DEFAULT_TIMEOUT_FOR_RESTART  = 1200
	STATUS_ERROR_ON_SELF_MANAGED = "Showing the status for externally configured %s is not supported."
)

type RestartCmdFlags struct {
	automate   bool
	chefServer bool
	opensearch bool
	postgresql bool
	node       string
	timeout    int
}

//	var restartServicesCmd = &cobra.Command{
//		Use:   "restart-services",
//		Short: "restart deployment services",
//		Long:  "Restart services for a deployment",
//		RunE:  runRestartServices,
//		Annotations: map[string]string{
//			docs.Tag: docs.FrontEnd,
//		},
//	}
var restartCmdFlags = RestartCmdFlags{}
var restartServicesCmd = &cobra.Command{

	Use:   "restart-services",
	Short: "restart deployment services",
	Long:  "Restart services for a deployment",
	RunE:  runRestartServicess(&restartCmdFlags),
	Annotations: map[string]string{
		docs.Tag: docs.FrontEnd,
	},
}

func init() {
	var restartCmdFlags = RestartCmdFlags{}

	restartServicesCmd.PersistentFlags().BoolVarP(&restartCmdFlags.automate, "automate", "a", false, "restart chef automate service on automate nodes")
	restartServicesCmd.PersistentFlags().BoolVar(&restartCmdFlags.automate, "a2", false, "restart chef automate service on automate nodes[DUPLICATE]")
	restartServicesCmd.PersistentFlags().BoolVarP(&restartCmdFlags.chefServer, "chef_server", "c", false, "restart chef automate service on chef-server nodes")
	restartServicesCmd.PersistentFlags().BoolVar(&restartCmdFlags.chefServer, "cs", false, "restart chef automate service on chef-server nodes[DUPLICATE]")
	restartServicesCmd.PersistentFlags().BoolVarP(&restartCmdFlags.opensearch, "opensearch", "o", false, "restart hab-sup service on opensearch nodes")
	restartServicesCmd.PersistentFlags().BoolVar(&restartCmdFlags.opensearch, "os", false, "restart hab-sup service on opensearch nodes[DUPLICATE]")
	restartServicesCmd.PersistentFlags().BoolVarP(&restartCmdFlags.postgresql, "postgresql", "p", false, "restart hab-sup service on postgresql nodes")
	restartServicesCmd.PersistentFlags().BoolVar(&restartCmdFlags.postgresql, "pg", false, "restart hab-sup service on postgresql nodes[DUPLICATE]")
	restartServicesCmd.PersistentFlags().StringVar(&restartCmdFlags.node, "node", "", "Node Ip address")
	restartServicesCmd.PersistentFlags().IntVar(&restartCmdFlags.timeout, "wait-timeout", DEFAULT_TIMEOUT_FOR_RESTART, "This flag sets the operation timeout duration (in seconds) for each individual node during the certificate rotation process")
	RootCmd.AddCommand(restartServicesCmd)
}

func runRestartServicess(flags *RestartCmdFlags) func(cmd *cobra.Command, args []string) error {
	return func(cmd *cobra.Command, args []string) error {
		return runRestartServices(cmd, args, flags)
	}
}

func runRestartServices(cmd *cobra.Command, args []string, flags *RestartCmdFlags) error {
	if isA2HARBFileExist() {
		//var restartCmdFlags = restartCmdFlags{}
		infra, err := getAutomateHAInfraDetails()
		if err != nil {
			return err
		}
		sshConfig := getSshDetails(infra)
		sshUtil := NewSSHUtil(sshConfig)
		if flags.timeout < DEFAULT_TIMEOUT_FOR_RESTART {
			return errors.Errorf("The operation timeout duration for each individual node during the services restart should be set to a value greater than %v seconds.", DEFAULT_TIMEOUT_FOR_RESTART)
		}
		sshConfig.timeout = flags.timeout
		sshUtil.setSSHConfig(sshConfig)

		if flags.automate || flags.chefServer || flags.postgresql || flags.opensearch {
			nodeMap := ConstructNodeMapForEachNodeTpe(infra,flags)
			cmdUtil := NewRemoteCmdExecutor(nodeMap, sshUtil, writer)
			_, err = cmdUtil.Execute()
		} else if !flags.automate && !flags.chefServer && !flags.postgresql && !flags.opensearch && flags.node == "" {
			//	nodeMap := constructAllNodeMap(infra)
			// cmdUtil := NewRemoteCmdExecutorWithoutNodeMap(sshUtil, writer)
			// _, err = cmdUtil.Execute()
			err = runStatusFromBastion(flags)
		} else {
			writer.Println(cmd.UsageString())
		}
		return err
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

func ConstructNodeMapForEachNodeTpe(infra *AutomateHAInfraDetails,flags *RestartCmdFlags) *NodeTypeAndCmd {
	//var restartCmdFlags = restartCmdFlags{}
	//RestartCmdFlags
	nodeMap := &NodeTypeAndCmd{
		Frontend: &Cmd{
			CmdInputs: &CmdInputs{
				NodeType: false,
			},
		},
		Automate: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      RESTART_FRONTEND_COMMAND,
				WaitTimeout:              flags.timeout,
				ErrorCheckEnableInOutput: true,
				Single:                   false,
				NodeIps:                  []string{flags.node},
				NodeType:                 flags.automate,
			},
		},
		ChefServer: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      RESTART_FRONTEND_COMMAND,
				WaitTimeout:              flags.timeout,
				ErrorCheckEnableInOutput: true,
				Single:                   false,
				NodeIps:                  []string{flags.node},
				NodeType:                 flags.chefServer,
			},
		},
		Postgresql: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      RESTART_BACKEND_COMMAND,
				WaitTimeout:              flags.timeout,
				ErrorCheckEnableInOutput: true,
				Single:                   false,
				NodeIps:                  []string{flags.node},
				NodeType:                 flags.postgresql,
			},
		},
		Opensearch: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      RESTART_BACKEND_COMMAND,
				WaitTimeout:              flags.timeout,
				ErrorCheckEnableInOutput: true,
				Single:                   false,
				NodeIps:                  []string{flags.node},
				NodeType:                 flags.opensearch,
			},
		},
		Infra: infra,
	}
	return nodeMap
}

func runStatusFromBastion(flags *RestartCmdFlags) error {

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
		go func(flags RestartCmdFlags, errChan chan<- error) {
			writer := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)
			sshUtil := NewSSHUtil(&SSHConfig{})
			nodeMap := constructNodeMapForAllNodeTypes(&flags, infra)
			cmdResult, err := executeRemoteExecutor(nodeMap, sshUtil, writer)
			errChan <- err
			wg.Add(1)
			go printStatusOutput(cmdResult, AUTOMATE, infra.Outputs.AutomatePrivateIps.Value, wg, mutex, writer)
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
			cmdResult, err := executeRemoteExecutor(nodeMap, sshUtil, writer)
			errChan <- err
			wg.Add(1)
			go printStatusOutput(cmdResult, CHEF_SERVER, infra.Outputs.ChefServerPrivateIps.Value, wg, mutex, writer)
		}(*flags, errChan)
	} else {
		errChan <- nil
	}

	if !isManagedServicesOn() {
		if flags.postgresql {
			go func(flags RestartCmdFlags, errChan chan<- error) {
				writer := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)
				sshUtil := NewSSHUtil(&SSHConfig{})
				flags.automate = false
				flags.chefServer = false
				flags.opensearch = false
				nodeMap := constructNodeMapForAllNodeTypes(&flags, infra)
				cmdResult, err := executeRemoteExecutor(nodeMap, sshUtil, writer)
				errChan <- err
				wg.Add(1)
				go printStatusOutput(cmdResult, POSTGRESQL, infra.Outputs.PostgresqlPrivateIps.Value, wg, mutex, writer)
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
				cmdResult, err := executeRemoteExecutor(nodeMap, sshUtil, writer)
				errChan <- err
				wg.Add(1)
				go printStatusOutput(cmdResult, OPENSEARCH, infra.Outputs.OpensearchPrivateIps.Value, wg, mutex, writer)
			}(*flags, errChan)
		} else {
			errChan <- nil
		}
	}

	n := 4
	if isManagedServicesOn() {
		n = 2
	}
	for i := 0; i < n; i++ {
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
	writer.Printf("=====================================================\n")
	for _, value := range cmdResult {
		for _, cmdResult := range value {
			printOutput(remoteService, *cmdResult, []string{}, writer)
		}
	}
	mutex.Unlock()
	wg.Done()
}

// func handleManagedServiceError(flags *restartCmdFlags) error {

// 	if flags.postgresql && flags.opensearch {
// 		return nil
// 	}

// 	if flags.postgresql {
// 		return status.Errorf(status.InvalidCommandArgsError, STATUS_ERROR_ON_SELF_MANAGED, POSTGRESQL)
// 	}

// 	if flags.opensearch {
// 		return status.Errorf(status.InvalidCommandArgsError, STATUS_ERROR_ON_SELF_MANAGED, OPENSEARCH)
// 	}

// 	return nil
// }

func constructNodeMapForAllNodeTypes(flags *RestartCmdFlags, infra *AutomateHAInfraDetails) *NodeTypeAndCmd {
	commandFrontEnd := RESTART_FRONTEND_COMMAND
	commandBackEnd := "sudo HAB_LICENSE=accept-no-persist systemctl restart hab-sup"
	nodeMap := &NodeTypeAndCmd{
		Frontend: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      commandFrontEnd,
				ErrorCheckEnableInOutput: true,
				WaitTimeout:              int(flags.timeout),
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
				WaitTimeout:              int(flags.timeout),
				NodeIps:                  []string{flags.node},
				NodeType:                 flags.automate,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true,
			},
		},
		ChefServer: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      commandFrontEnd,
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
				Cmd:                      commandBackEnd,
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
				Cmd:                      commandBackEnd,
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
