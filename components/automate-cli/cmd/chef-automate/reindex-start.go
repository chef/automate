package main

import (
	"context"
	"os"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/spf13/cobra"
	"google.golang.org/grpc"
)

var reindexCmd = &cobra.Command{
	Use:   "reindex",
	Short: "Manage OpenSearch reindexing",
}

// reindexStartCmd triggers the reindexing process
var reindexStartCmd = &cobra.Command{
	Use:               "start",
	Short:             "Start OpenSearch reindexing",
	RunE:              runReindexStartCmd,
	PersistentPreRunE: preReindexCmd,
}

// ReindexFlow manages the CLI flow for reindexing
type ReindexFlow struct {
	DsClient ReindexDSClient
	Writer   *cli.Writer
}

// ReindexDSClient defines the required RPC methods for deployment-service
type ReindexDSClient interface {
	StartReindex(ctx context.Context, in *api.StartReindexRequest, opts ...grpc.CallOption) (*api.StartReindexResponse, error)
	Close() error
}

// runReindexStartCmd connects to deployment-service and triggers reindexing
func runReindexStartCmd(cmd *cobra.Command, args []string) error {
	rf, err := NewReindexFlow(writer)
	if err != nil {
		return err
	}
	return rf.StartReindex()
}

// NewReindexFlow establishes a connection to deployment-service
func NewReindexFlow(writer *cli.Writer) (*ReindexFlow, error) {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return nil, status.Wrap(err, status.DeploymentServiceCallError, "Failed to establish connection to deployment-service")
	}
	return &ReindexFlow{DsClient: connection, Writer: writer}, nil
}

// StartReindex triggers reindexing via deployment-service
func (rf *ReindexFlow) StartReindex() error {
	defer rf.DsClient.Close()

	req := &api.StartReindexRequest{} // Modify if additional params are needed

	_, err := rf.DsClient.StartReindex(context.Background(), req)
	if err != nil {
		return status.Wrap(err, status.DeploymentServiceCallError, "Failed to start reindexing")
	}

	rf.Writer.Println("Reindexing started successfully.")
	return nil
}

// preReindexCmd performs pre-run validation before executing the command
func preReindexCmd(cmd *cobra.Command, args []string) error {
	err := commandPrePersistent(cmd)
	if err != nil {
		return status.Wrap(err, status.CommandExecutionError, "unable to set command parent settings")
	}

	// If running in an A2HA setup, execute on a single node and exit
	if isA2HARBFileExist() {
		output, err := RunCmdOnSingleAutomateNode(cmd, args)
		if err != nil {
			return err
		}
		writer.Print(output)
		os.Exit(0) // Prevents further execution in HA mode
	}

	return nil
}

// Attach reindexStartCmd to reindexCmd
func init() {
	reindexCmd.AddCommand(reindexStartCmd)
	RootCmd.AddCommand(reindexCmd)
}
