package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/spf13/cobra"
	"google.golang.org/grpc"
)

// reindexCmd is the parent command
var reindexCmd = &cobra.Command{
	Use:   "reindex COMMAND",
	Short: "Manage OpenSearch reindexing",
}

// reindexStatusCmd retrieves reindexing status
var reindexStatusCmd = &cobra.Command{
	Use:               "status",
	Short:             "Retrieve the current reindexing status",
	RunE:              runReindexStatusCmd,
	PersistentPreRunE: preReindexCmd,
}

// ReindexDSClient defines the required RPC methods for deployment-service
type ReindexDSClient interface {
	GetReindexStatus(ctx context.Context, in *api.GetReindexStatusRequest, opts ...grpc.CallOption) (*api.GetReindexStatusResponse, error)
	Close() error
}

// ReindexFlow manages the CLI flow for reindexing
type ReindexFlow struct {
	DsClient ReindexDSClient
	Writer   *cli.Writer
}

// runReindexStatusCmd connects to deployment-service and fetches reindex status
func runReindexStatusCmd(cmd *cobra.Command, args []string) error {
	rf, err := NewReindexFlow(writer)
	if err != nil {
		return err
	}
	return rf.GetReindexStatus()
}

// NewReindexFlow establishes a connection to deployment-service
func NewReindexFlow(writer *cli.Writer) (*ReindexFlow, error) {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return nil, err
	}
	return &ReindexFlow{DsClient: connection, Writer: writer}, nil
}

// GetReindexStatus fetches and prints the reindexing status
func (rf *ReindexFlow) GetReindexStatus() error {
	defer rf.DsClient.Close()

	req := &api.GetReindexStatusRequest{RequestId: 1} // Adjust as needed

	resp, err := rf.DsClient.GetReindexStatus(context.Background(), req)
	if err != nil {
		return status.Wrap(err, status.DeploymentServiceCallError, "Failed to get reindex status")
	}

	// Parse and print the response
	var statusData map[string]interface{}
	err = json.Unmarshal([]byte(resp.StatusJson), &statusData)
	if err != nil {
		return fmt.Errorf("failed to parse JSON response: %w", err)
	}

	fmt.Println("Reindex Status:")
	printReindexStatus(statusData)

	return nil
}

// printReindexStatus formats and prints the reindex status
func printReindexStatus(statusData map[string]interface{}) {
	overallStatus, _ := statusData["overall_status"].(string)
	fmt.Printf("Overall Status: %s\n", overallStatus)

	indexes, ok := statusData["indexes"].([]interface{})
	if !ok {
		fmt.Println("No index details available.")
		return
	}

	fmt.Println("Index Details:")
	for _, index := range indexes {
		if indexMap, ok := index.(map[string]interface{}); ok {
			fmt.Printf("- Index: %s, Stage: %s\n", indexMap["index"], indexMap["stage"])
		}
	}
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

func init() {
	reindexCmd.AddCommand(reindexStatusCmd)
	RootCmd.AddCommand(reindexCmd)
}
