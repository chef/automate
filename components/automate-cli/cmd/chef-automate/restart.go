package main

import (
	"context"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
)

const (
	RESTART_FRONTEND_COMMAND    = `sudo chef-automate restart-services`
	RESTART_BACKEND_COMMAND     = `sudo systemctl restart hab-sup`
	DEFAULT_TIMEOUT_FOR_RESTART = 1500
)

var restartCmdFlags = struct {
	automate    bool
	chefServer  bool
	opensearch  bool
	postgresql  bool
	timeout     int
	waitTimeout int
}{}

var restartServicesCmd = &cobra.Command{
	Use:   "restart-services",
	Short: "restart deployment services",
	Long:  "Restart services for a deployment",
	RunE:  runRestartServices,
	Annotations: map[string]string{
		docs.Tag: docs.FrontEnd,
	},
}

func init() {
	restartServicesCmd.PersistentFlags().BoolVarP(&restartCmdFlags.automate, "automate", "a", false, "restart chef automate service on automate nodes")
	restartServicesCmd.PersistentFlags().BoolVar(&restartCmdFlags.automate, "a2", false, "restart chef automate service on automate nodes[DUPLICATE]")
	restartServicesCmd.PersistentFlags().BoolVarP(&restartCmdFlags.chefServer, "chef_server", "c", false, "restart chef automate service on chef-server nodes")
	restartServicesCmd.PersistentFlags().BoolVar(&restartCmdFlags.chefServer, "cs", false, "restart chef automate service on chef-server nodes[DUPLICATE]")
	restartServicesCmd.PersistentFlags().BoolVarP(&restartCmdFlags.opensearch, "opensearch", "o", false, "restart hab-sup service on opensearch nodes")
	restartServicesCmd.PersistentFlags().BoolVar(&restartCmdFlags.opensearch, "os", false, "restart hab-sup service on opensearch nodes[DUPLICATE]")
	restartServicesCmd.PersistentFlags().BoolVarP(&restartCmdFlags.postgresql, "postgresql", "p", false, "restart hab-sup service on postgresql nodes")
	restartServicesCmd.PersistentFlags().BoolVar(&restartCmdFlags.postgresql, "pg", false, "restart hab-sup service on postgresql nodes[DUPLICATE]")
	restartServicesCmd.PersistentFlags().IntVar(&restartCmdFlags.timeout, "wait-timeout", DEFAULT_TIMEOUT_FOR_RESTART, "This flag sets the operation timeout duration (in seconds) for each individual node during the certificate rotation process")
	RootCmd.AddCommand(restartServicesCmd)
}

func runRestartServices(cmd *cobra.Command, args []string) error {
	if isA2HARBFileExist() {
		infra, err := getAutomateHAInfraDetails()
		if err != nil {
			return err
		}
		frontend := &Cmd{
			CmdInputs: &CmdInputs{
				NodeType: false,
			},
		}

		automate := &Cmd{
			PreExec: nil,
			CmdInputs: &CmdInputs{
				Cmd:                      RESTART_FRONTEND_COMMAND,
				WaitTimeout:              restartCmdFlags.waitTimeout,
				Single:                   false,
				Args:                     args,
				ErrorCheckEnableInOutput: true,
				//SkipPrintOutput:          true,
				NodeType:                 restartCmdFlags.automate,
			},
		}

		chefServer := &Cmd{
			PreExec: nil,
			CmdInputs: &CmdInputs{
				Cmd:                      RESTART_FRONTEND_COMMAND,
				WaitTimeout:              restartCmdFlags.waitTimeout,
				Single:                   false,
				Args:                     args,
				ErrorCheckEnableInOutput: true,
				//SkipPrintOutput:          true,
				NodeType:                 restartCmdFlags.chefServer,
			},
		}

		postgresql := &Cmd{
			PreExec: nil,
			CmdInputs: &CmdInputs{
				Cmd:                      RESTART_BACKEND_COMMAND,
				WaitTimeout:              restartCmdFlags.waitTimeout,
				Single:                   false,
				Args:                     args,
				ErrorCheckEnableInOutput: true,
				NodeType:                 restartCmdFlags.postgresql,
			},
		}
		opensearch := &Cmd{
			PreExec: nil,
			CmdInputs: &CmdInputs{
				Cmd:                      RESTART_BACKEND_COMMAND,
				WaitTimeout:              restartCmdFlags.waitTimeout,
				Single:                   false,
				Args:                     args,
				ErrorCheckEnableInOutput: true,
				NodeType:                 restartCmdFlags.opensearch,
			},
		}
		nodeMap := &NodeTypeAndCmd{
			Frontend:   frontend,
			Automate:   automate,
			ChefServer: chefServer,
			Postgresql: postgresql,
			Opensearch: opensearch,
			Infra:      infra,
		}
		if restartCmdFlags.timeout < DEFAULT_TIMEOUT_FOR_RESTART {
			return errors.Errorf("The operation timeout duration for each individual node during the services restart should be set to a value greater than %v seconds.", DEFAULT_TIMEOUT_FOR_RESTART)
		}
		sshConfig := getSshDetails(infra)
		sshConfig.timeout = restartCmdFlags.timeout
		sshUtil := NewSSHUtil(sshConfig)
		//sshUtil.setSSHConfig(sshConfig)

		cmdUtil := NewRemoteCmdExecutor(nodeMap, sshUtil, writer)

		if restartCmdFlags.automate || restartCmdFlags.chefServer || restartCmdFlags.postgresql || restartCmdFlags.opensearch {
			_, err = cmdUtil.Execute()
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
