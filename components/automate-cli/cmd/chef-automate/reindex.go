package main

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"os"

	"github.com/chef/automate/api/external/ingest"
	"github.com/chef/automate/api/external/ingest/request"
	"github.com/chef/automate/components/automate-cli/pkg/status"
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

// runReindexStatusCmd connects to ingest-service and fetches reindex status
func runReindexStatusCmd(cmd *cobra.Command, args []string) error {
	// Connect to ingest-service
	conn, err := grpc.Dial("localhost:10122", grpc.WithInsecure())
	if err != nil {
		log.Fatalf("Failed to connect to ingest service: %v", err)
	}
	defer conn.Close()

	client := ingest.NewChefIngesterServiceClient(conn)

	// Create request with a hardcoded RequestId for now
	req := &request.GetReindexStatusRequest{RequestId: 1}
	resp, err := client.GetReindexStatus(context.Background(), req)
	if err != nil {
		log.Fatalf("Failed to get reindex status: %v", err)
	}

	// Parse and print the response
	var statusData map[string]interface{}
	err = json.Unmarshal([]byte(resp.StatusJson), &statusData)
	if err != nil {
		log.Fatalf("Failed to parse JSON response: %v", err)
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
