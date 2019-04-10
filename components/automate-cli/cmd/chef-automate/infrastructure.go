package main

import (
	"context"

	"github.com/gofrs/uuid"
	"github.com/spf13/cobra"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
)

func init() {
	infrastructureCmd.AddCommand(nodeDeleteCmd)

	RootCmd.AddCommand(infrastructureCmd)
}

var infrastructureCmd = &cobra.Command{
	Use:   "infrastructure COMMAND",
	Short: "Chef Automate infrastructure",
	Long:  "Commands for automation infrastructure management, for data related to chef-client runs and chef-server actions.",
}

var nodeDeleteCmd = &cobra.Command{
	Use:   "node-delete [uuid]",
	Short: "Delete node by node uuid",
	Long:  "",
	RunE:  runDeleteNodeCmd,
	Args:  cobra.ExactArgs(1),
}

func runDeleteNodeCmd(cmd *cobra.Command, args []string) error {
	connection, err := client.Connection(client.DefaultClientTimeout)

	if err != nil {
		return err
	}

	defer func() {
		_ = connection.Close()
	}()

	nodeID := args[0]
	if !isValidUUID(nodeID) {
		return status.New(status.InvalidCommandArgsError, "argument in not a valid node UUID")
	}
	deleteReq := &api.InfrastructureNodeDeleteRequest{NodeId: nodeID}

	_, err = connection.InfrastructureNodeDelete(context.Background(), deleteReq)
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to delete node failed",
		)
	}

	writer.Println("Node successfully deleted")
	return nil
}

func isValidUUID(id string) bool {
	_, err := uuid.FromString(id)
	return err == nil
}
