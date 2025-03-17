package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"google.golang.org/grpc"
)

var reindexCmd = &cobra.Command{
	Use:               "reindex COMMAND",
	Short:             "Manage OpenSearch reindexing",
	Long:              "Commands for managing OpenSearch reindexing within Chef Automate.",
	PersistentPreRunE: preReindexCmd,
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

var reindexStartCmd = &cobra.Command{
	Use:               "start",
	Short:             "Start OpenSearch reindexing",
	PersistentPreRunE: checkLicenseStatusForExpiry, // Enforce license check before execution
	RunE:              runReindexStartCmd,
	Args:              cobra.NoArgs,
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

var reindexStatusCmd = &cobra.Command{
	Use:               "status",
	Short:             "Retrieve the current reindexing status",
	PersistentPreRunE: checkLicenseStatusForExpiry, // Enforce license check before execution
	RunE:              runReindexStatusCmd,
	Args:              cobra.NoArgs,
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

type ReindexDSClient interface {
	GetReindexStatus(ctx context.Context, in *api.GetReindexStatusRequest, opts ...grpc.CallOption) (*api.GetReindexStatusResponse, error)
	StartReindex(ctx context.Context, in *api.StartReindexRequest, opts ...grpc.CallOption) (*api.StartReindexResponse, error)
	Close() error
}

// ReindexFlow manages the CLI flow for reindexing
type ReindexFlow struct {
	DsClient ReindexDSClient
	Writer   *cli.Writer
}

func runReindexStatusCmd(cmd *cobra.Command, args []string) error {
	rf, err := NewReindexFlow(writer)
	if err != nil {
		return err
	}
	return rf.GetReindexStatus()
}

func (rf *ReindexFlow) GetReindexStatus() error {
	defer rf.DsClient.Close()
	logrus.Info("Fetching reindex status...")
	req := &api.GetReindexStatusRequest{RequestId: 0} // Pass 0 to trigger latest request ID lookup

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

	printReindexStatus(statusData)

	return nil
}

// printReindexStatus formats and prints the reindex status
func printReindexStatus(statusData map[string]interface{}) {
	requestID, _ := statusData["request_id"].(float64)
	overallStatus, _ := statusData["status"].(string)
	fmt.Printf("Request ID: %.0f\n", requestID)
	fmt.Printf("Overall Status: %s\n", overallStatus)

	indexes, ok := statusData["indexes"].([]interface{})
	if !ok {
		fmt.Println("No index details available.")
		return
	}

	fmt.Println("Index Details:")
	for _, index := range indexes {
		if indexMap, ok := index.(map[string]interface{}); ok {
			fmt.Printf("- Index: %s, Stage: %s, Status: %s\n",
				indexMap["index"], indexMap["stage"], indexMap["status"])
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

// runReindexStartCmd connects to deployment-service and triggers reindexing
func runReindexStartCmd(cmd *cobra.Command, args []string) error {
	rf, err := NewReindexFlow(writer)
	if err != nil {
		return err
	}
	return rf.StartReindex()
}

func NewReindexFlow(writer *cli.Writer) (*ReindexFlow, error) {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return nil, status.Wrap(err, status.DeploymentServiceCallError, "Failed to establish connection to deployment-service")
	}
	return &ReindexFlow{
		DsClient: connection,
		Writer:   writer,
	}, nil
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

// Attach reindexStartCmd to reindexCmd
func init() {
	reindexCmd.AddCommand(reindexStatusCmd)
	reindexCmd.AddCommand(reindexStartCmd)
	RootCmd.AddCommand(reindexCmd)
}
