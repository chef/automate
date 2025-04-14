package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"google.golang.org/grpc"
)

const (
	enable_maintenance_mode_cmd = `chef-automate maintenance on`
	maintenanceModeMsg          = "This reindex put the system in maintenance mode. During that period no new ingestion of data can happen. \n The maintenance mode will be switched off automatically at the end of a successful reindexing. But in case of an unsuccessful upgrade, you have to set it ‘Off’ manually.\nAre you ready to proceed?"
	downTimeError               = "There will be a downtime while reindexing. Please prepare for down time and run the reindex"
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
	GetEligilbeIndexes(ctx context.Context, re *api.GetEligilbeIndexesRequest, opts ...grpc.CallOption) (*api.GetEligilbeIndexesResponse, error)
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

	//Check if indexes are there to reindex
	indexes, err := rf.DsClient.GetEligilbeIndexes(context.Background(), &api.GetEligilbeIndexesRequest{})
	if err != nil {
		return status.Wrap(err, status.DeploymentServiceCallError, "Failed to get the indexes for reindexing")
	}

	if indexes == nil || len(indexes.Indexes) == 0 {
		rf.Writer.Println("No indexes available for reindexing.")
		return nil
	}

	//Ask for the confirmation for maintenance mode
	resp, err := rf.Writer.Confirm(maintenanceModeMsg)
	if err != nil {
		rf.Writer.Error(err.Error())
		return status.Errorf(status.InvalidCommandArgsError, err.Error())
	}
	if !resp {
		rf.Writer.Error(downTimeError)
		return status.New(status.InvalidCommandArgsError, downTimeError)
	}

	out, err := exec.Command("/bin/sh", "-c", enable_maintenance_mode_cmd).Output()
	if !strings.Contains(string(out), "Updating deployment configuration") || err != nil {
		rf.Writer.Errorln("error in enabling the maintenance mode")
		return nil
	}

	_, err = rf.DsClient.StartReindex(context.Background(), req)
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
