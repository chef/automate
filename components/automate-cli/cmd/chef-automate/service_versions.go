// Copyright © 2018 Chef Software

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
)

const (
	SERVICE_VERSIONS_ERROR_ON_SELF_MANAGED = "Showing the service-versions for externally configured %s is not supported."
	BACKEND_SERVICE_VERSIONS_CMD           = `sudo HAB_LICENSE=accept-no-persist hab svc status | awk 'NR>1 {split($1,a,"/"); printf "%s/%s %s %s\n", a[1], a[2], a[3], a[4]}'`
	FRONTEND_SERVICE_VERSIONS_CMD          = "sudo chef-automate service-versions"
)

type ServiceVersionsCmdFlags struct {
	automate   bool
	chefServer bool
	postgresql bool
	opensearch bool
	node       string
}

var serviceVersionsCmdFlag = ServiceVersionsCmdFlags{}

type ServiceVersionsCmdResult struct {
	cmdResult map[string][]*CmdResult
	writer    *cli.Writer
	nodeType  string
	err       error
}

func init() {
	var serviceVersionsCmd = &cobra.Command{
		Use:   "service-versions",
		Short: "Retrieve the versions of the individual Chef Automate services",
		Long:  "Retrieve the versions of the individual Chef Automate services",
		RunE:  runServiceVersionsCmd,
		Annotations: map[string]string{
			docs.Tag: docs.BastionHost,
		},
	}
	serviceVersionsCmd.PersistentFlags().BoolVarP(&serviceVersionsCmdFlag.automate, "automate", "a", false, "Shows service-versions for Automate nodes")
	serviceVersionsCmd.PersistentFlags().SetAnnotation("automate", docs.Compatibility, []string{docs.CompatiblewithHA})
	serviceVersionsCmd.PersistentFlags().BoolVarP(&serviceVersionsCmdFlag.chefServer, "chef_server", "c", false, "Shows service-versions for Chef-server nodes")
	serviceVersionsCmd.PersistentFlags().SetAnnotation("chef_server", docs.Compatibility, []string{docs.CompatiblewithHA})
	serviceVersionsCmd.PersistentFlags().BoolVarP(&serviceVersionsCmdFlag.postgresql, "postgresql", "p", false, "Shows service-versions for PostgresQL nodes")
	serviceVersionsCmd.PersistentFlags().SetAnnotation("postgresql", docs.Compatibility, []string{docs.CompatiblewithHA})
	serviceVersionsCmd.PersistentFlags().BoolVarP(&serviceVersionsCmdFlag.opensearch, "opensearch", "o", false, "Shows service-versions for OpenSearch nodes")
	serviceVersionsCmd.PersistentFlags().SetAnnotation("opensearch", docs.Compatibility, []string{docs.CompatiblewithHA})
	serviceVersionsCmd.PersistentFlags().BoolVar(&serviceVersionsCmdFlag.automate, "a2", false, "Shows service-versions for Automate nodes[DUPLICATE]")
	serviceVersionsCmd.PersistentFlags().SetAnnotation("a2", docs.Compatibility, []string{docs.CompatiblewithHA})
	serviceVersionsCmd.PersistentFlags().BoolVar(&serviceVersionsCmdFlag.chefServer, "cs", false, "Shows service-versions for Chef-server nodes[DUPLICATE]")
	serviceVersionsCmd.PersistentFlags().SetAnnotation("cs", docs.Compatibility, []string{docs.CompatiblewithHA})
	serviceVersionsCmd.PersistentFlags().BoolVar(&serviceVersionsCmdFlag.postgresql, "pg", false, "Shows service-versions for PostgresQL nodes[DUPLICATE]")
	serviceVersionsCmd.PersistentFlags().SetAnnotation("pg", docs.Compatibility, []string{docs.CompatiblewithHA})
	serviceVersionsCmd.PersistentFlags().BoolVar(&serviceVersionsCmdFlag.opensearch, "os", false, "Shows service-versions for OpenSearch nodes[DUPLICATE]")
	serviceVersionsCmd.PersistentFlags().SetAnnotation("os", docs.Compatibility, []string{docs.CompatiblewithHA})
	serviceVersionsCmd.PersistentFlags().StringVar(&serviceVersionsCmdFlag.node, "node", "", "Pass this flag to check service-versions of particular node in the cluster")
	serviceVersionsCmd.PersistentFlags().SetAnnotation("node", docs.Compatibility, []string{docs.CompatiblewithHA})
	RootCmd.AddCommand(serviceVersionsCmd)
}

func runServiceVersionsCmd(cmd *cobra.Command, args []string) error {
	if isA2HARBFileExist() {
		nodeOpUtils := &NodeUtilsImpl{}
		remoteExecutor := NewRemoteCmdExecutorWithoutNodeMap(&SSHUtilImpl{}, cli.NewWriter(os.Stdout, os.Stderr, os.Stdin))
		if err := isFlagSet(cmd, &serviceVersionsCmdFlag); err != nil {
			return err
		}
		return runServiceVersionsFromBastion(&serviceVersionsCmdFlag, nodeOpUtils, remoteExecutor, printServiceVersionsOutput)
	}
	if err := runServiceVersionsOnStandalone(); err != nil {
		return err
	}

	return nil
}

func runServiceVersionsFromBastion(flags *ServiceVersionsCmdFlags, nodeOpUtils NodeOpUtils, remoteExe RemoteCmdExecutor, printServiceVersionsOutput func(map[string][]*CmdResult, string, *cli.Writer)) error {

	infra, _, err := nodeOpUtils.getHaInfraDetails()
	if err != nil {
		return err
	}
	serviceVersionsCmdResults := make(chan ServiceVersionsCmdResult, 4)

	runServiceVersionsCmdForFrontEnd(flags, infra, serviceVersionsCmdResults, remoteExe)

	if nodeOpUtils.isManagedServicesOn() {
		serviceVersionsCmdResults <- ServiceVersionsCmdResult{}
		serviceVersionsCmdResults <- ServiceVersionsCmdResult{}
		err = getValueFromResultChannel(serviceVersionsCmdResults, printServiceVersionsOutput)
		if err != nil {
			return err
		}
		return handleManagedServiceErrorForServiceVersionsCmd(flags)
	}

	runServiceVersionsCmdForBackend(flags, infra, serviceVersionsCmdResults, remoteExe)

	return getValueFromResultChannel(serviceVersionsCmdResults, printServiceVersionsOutput)
}

func runServiceVersionsCmdForFrontEnd(flags *ServiceVersionsCmdFlags, infra *AutomateHAInfraDetails, serviceVersionsCmdResults chan ServiceVersionsCmdResult, remoteExe RemoteCmdExecutor) {

	if flags.automate {
		go nodeServiceVersions(*flags, AUTOMATE, infra, serviceVersionsCmdResults, remoteExe)
	} else {
		serviceVersionsCmdResults <- ServiceVersionsCmdResult{}
	}
	if flags.chefServer {
		flags.automate = false
		go nodeServiceVersions(*flags, CHEF_SERVER, infra, serviceVersionsCmdResults, remoteExe)
	} else {
		serviceVersionsCmdResults <- ServiceVersionsCmdResult{}
	}
}
func getValueFromResultChannel(serviceVersionsCmdResults chan ServiceVersionsCmdResult, printServiceVersionsOutput func(map[string][]*CmdResult, string, *cli.Writer)) error {
	for i := 0; i < 4; i++ {
		cmdResult := <-serviceVersionsCmdResults
		if cmdResult.err != nil {
			return cmdResult.err
		} else {
			if cmdResult.writer != nil {
				printServiceVersionsOutput(cmdResult.cmdResult, cmdResult.nodeType, cmdResult.writer)
			}
		}
	}
	return nil
}

func handleManagedServiceErrorForServiceVersionsCmd(flags *ServiceVersionsCmdFlags) error {

	if flags.postgresql && flags.opensearch {
		return status.Errorf(status.InvalidCommandArgsError, SERVICE_VERSIONS_ERROR_ON_SELF_MANAGED, POSTGRESQL+" and "+OPENSEARCH)
	}

	if flags.postgresql {
		return status.Errorf(status.InvalidCommandArgsError, SERVICE_VERSIONS_ERROR_ON_SELF_MANAGED, POSTGRESQL)
	}

	if flags.opensearch {
		return status.Errorf(status.InvalidCommandArgsError, SERVICE_VERSIONS_ERROR_ON_SELF_MANAGED, OPENSEARCH)
	}

	return nil
}
func runServiceVersionsCmdForBackend(flags *ServiceVersionsCmdFlags, infra *AutomateHAInfraDetails, serviceVersionsCmdResults chan ServiceVersionsCmdResult, remoteExe RemoteCmdExecutor) {

	if flags.postgresql {
		flags.automate = false
		flags.chefServer = false
		go nodeServiceVersions(*flags, POSTGRESQL, infra, serviceVersionsCmdResults, remoteExe)
	} else {
		serviceVersionsCmdResults <- ServiceVersionsCmdResult{}
	}

	if flags.opensearch {
		flags.automate = false
		flags.chefServer = false
		flags.postgresql = false
		go nodeServiceVersions(*flags, OPENSEARCH, infra, serviceVersionsCmdResults, remoteExe)
	} else {
		serviceVersionsCmdResults <- ServiceVersionsCmdResult{}
	}

}

func nodeServiceVersions(flags ServiceVersionsCmdFlags, nodeType string, infra *AutomateHAInfraDetails, serviceVersionsCmdResults chan ServiceVersionsCmdResult, remoteExe RemoteCmdExecutor) {
	writer := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)
	remoteExe.SetWriter(writer)
	nodeMap := constructNodeMapForServiceVersions(infra, &flags)
	cmdResult, err := remoteExe.ExecuteWithNodeMap(nodeMap)
	serviceVersionsCmdResults <- ServiceVersionsCmdResult{
		cmdResult: cmdResult,
		writer:    writer,
		nodeType:  nodeType,
		err:       err,
	}
}

func constructNodeMapForServiceVersions(infra *AutomateHAInfraDetails, flags *ServiceVersionsCmdFlags) *NodeTypeAndCmd {
	nodeMap := &NodeTypeAndCmd{
		Frontend: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      FRONTEND_SERVICE_VERSIONS_CMD,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true,
			},
		},
		Automate: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      FRONTEND_SERVICE_VERSIONS_CMD,
				ErrorCheckEnableInOutput: true,
				NodeIps:                  []string{flags.node},
				NodeType:                 flags.automate,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true,
			},
		},
		ChefServer: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      FRONTEND_SERVICE_VERSIONS_CMD,
				ErrorCheckEnableInOutput: true,
				NodeIps:                  []string{flags.node},
				NodeType:                 flags.chefServer,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true,
			},
		},
		Postgresql: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      BACKEND_SERVICE_VERSIONS_CMD,
				NodeIps:                  []string{flags.node},
				ErrorCheckEnableInOutput: true,
				NodeType:                 flags.postgresql,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true,
			},
		},
		Opensearch: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      BACKEND_SERVICE_VERSIONS_CMD,
				NodeIps:                  []string{flags.node},
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

func printServiceVersionsOutput(cmdResult map[string][]*CmdResult, remoteService string, writer *cli.Writer) {
	writer.Printf("\n=====================================================%s =======================================================\n", " Service-Versions on "+remoteService)

	for _, value := range cmdResult {
		for _, cmdResult := range value {
			if cmdResult.Error != nil {
				printServiceVersionsErrorOutput(cmdResult, remoteService, writer)
			} else {
				writer.Printf("Output for Host IP %s : \n%s", cmdResult.HostIP, cmdResult.Output+"\n")
				writer.Success("Command is executed on " + remoteService + " node : " + cmdResult.HostIP + "\n")
			}
			writer.BufferWriter().Flush()
		}
	}
}

func printServiceVersionsErrorOutput(cmdResult *CmdResult, remoteService string, writer *cli.Writer) {
	isOutputError := false

	if strings.Contains(cmdResult.Output, "DeploymentServiceUnreachableError") {
		isOutputError = true
		writer.Failf(CMD_FAIL_MSG, remoteService, cmdResult.HostIP, cmdResult.Output)
	}

	if !isOutputError {
		writer.Failf(CMD_FAIL_MSG, remoteService, cmdResult.HostIP, cmdResult.Error.Error())
	}
}

func isFlagSet(cmd *cobra.Command, flags *ServiceVersionsCmdFlags) error {
	if !flags.automate && !flags.chefServer && !flags.opensearch && !flags.postgresql {
		if len(flags.node) != 0 {
			return status.Errorf(status.InvalidCommandArgsError, "Please provide service flag")
		}
		writer.Println(cmd.UsageString())
		return errors.New("No flag is enabled. Please provide any flag")
	}
	return nil
}

func runServiceVersionsOnStandalone() error {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return err
	}

	response, err := connection.ServiceVersions(context.Background(), &api.ServiceVersionsRequest{})
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to retrieve service versions failed",
		)
	}
	for _, s := range response.Services {
		writer.Printf("%s/%s %s %s\n", s.Origin, s.Name, s.Version, s.Release)
	}
	return nil
}
