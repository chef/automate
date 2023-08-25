package main

import (
	"context"
	"os"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/gofrs/uuid"
	"github.com/spf13/cobra"
	"google.golang.org/grpc"
)

func init() {
	infrastructureCmd.AddCommand(nodeDeleteCmd)

	RootCmd.AddCommand(infrastructureCmd)
}

var infrastructureCmd = &cobra.Command{
	Use:               "infrastructure COMMAND",
	Short:             "Chef Automate infrastructure",
	Long:              "Commands for automation infrastructure management, for data related to chef-client runs and chef-server actions.",
	PersistentPreRunE: preInfrastructureCmd,
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

var nodeDeleteCmd = &cobra.Command{
	Use:   "node-delete [uuid]",
	Short: "Delete node by node uuid",
	Long:  "",
	RunE:  runDeleteNodeCmd,
	Args:  cobra.ExactArgs(1),
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

type DSClient interface {
	InfrastructureNodeDelete(ctx context.Context, in *api.InfrastructureNodeDeleteRequest, opts ...grpc.CallOption) (*api.InfrastructureNodeDeleteResponse, error)
	Close() error
}

type InfraFlow struct {
	DsClient DSClient
	Writer   *cli.Writer
}

func runDeleteNodeCmd(cmd *cobra.Command, args []string) error {
	ifw, err := NewDeleteNode(writer)
	if err != nil {
		return err
	}
	return ifw.RunDeleteNode(args[0])
}

func preInfrastructureCmd(cmd *cobra.Command, args []string) error {
	err := commandPrePersistent(cmd)
	if err != nil {
		return status.Wrap(err, status.CommandExecutionError, "unable to set command parent settings")
	}
	if isA2HARBFileExist() {
		output, err := RunCmdOnSingleAutomateNode(cmd, args)
		if err != nil {
			return err
		}
		writer.Print(output)
		os.Exit(0)
	}
	return nil
}

func NewDeleteNode(writer *cli.Writer) (*InfraFlow, error) {
	connection, err := client.Connection(client.DefaultClientTimeout)

	if err != nil {
		return nil, err
	}
	return &InfraFlow{DsClient: connection, Writer: writer}, nil
}

func (ifw *InfraFlow) RunDeleteNode(nodeID string) error {

	defer ifw.DsClient.Close()

	if !isValidUUID(nodeID) {
		return status.New(status.InvalidCommandArgsError, "argument in not a valid node UUID")
	}
	deleteReq := &api.InfrastructureNodeDeleteRequest{NodeId: nodeID}

	_, err := ifw.DsClient.InfrastructureNodeDelete(context.Background(), deleteReq)
	if err != nil {
		return status.Wrap(err, status.DeploymentServiceCallError, "Request to delete node failed")
	}

	ifw.Writer.Println("Node successfully deleted")
	return nil
}

func isValidUUID(id string) bool {
	_, err := uuid.FromString(id)
	return err == nil
}
