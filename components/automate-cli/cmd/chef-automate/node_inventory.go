package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"time"

	"github.com/spf13/cobra"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
)

func init() {
	nodeInventory.PersistentFlags().BoolVarP(
		&nodeInventoryCmdFlags.overwriteFile,
		"overwrite",
		"o",
		false,
		"Overwrite existing outfile if another file exists")

	RootCmd.AddCommand(nodeInventory)
}

var nodeInventory = &cobra.Command{
	Use:   "node-inventory [/path/to/outfile.json]",
	Short: "Display node inventory data",
	Long: "Display node inventory data.\n\n" +
		"Print out all the nodes that currently exist in Automate:\n\n",
	RunE:       runNodeInventory,
	Deprecated: "The node-inventory command will be removed after 30 November 2018.",
}

var nodeInventoryCmdFlags = struct {
	overwriteFile bool
}{}

type nodeInventoryResult struct {
	Nodes     []*api.InventoryNode `json:"nodes"`
	NodeCount int                  `json:"node_count"`
}

func runNodeInventory(cmd *cobra.Command, args []string) error {
	nodes, err := getInventoryNodes()
	if err != nil {
		return err
	}

	status.GlobalResult = nodeInventoryResult{
		Nodes:     nodes.Nodes,
		NodeCount: nodes.NodeCount,
	}

	nodesJson, err := json.Marshal(nodes)
	if err != nil {
		return status.Annotate(err, status.MarshalError)
	}

	if len(args) > 0 && args[0] != "" {
		outFile, err := filepath.Abs(args[0])
		if err != nil {
			return status.Annotate(err, status.FileAccessError)
		}

		if _, err := os.Stat(outFile); err == nil {
			if !nodeInventoryCmdFlags.overwriteFile {
				ok, err := writer.Confirm(fmt.Sprintf("%s file already exists. Do you wish to overwrite it?", outFile))
				if err != nil {
					return status.Annotate(err, status.FileAccessError)
				}
				if !ok {
					return status.New(status.FileAccessError, "Node inventory file cannot be overwritten")
				}
			}
		}

		f, err := os.Create(outFile)
		if err != nil {
			return status.Annotate(err, status.FileAccessError)
		}
		defer func() {
			_ = f.Close()
		}()

		_, err = f.Write(nodesJson)
		if err != nil {
			return status.Annotate(err, status.FileAccessError)
		}

		return nil
	}

	_, err = writer.Write(nodesJson)
	return err
}

type inventoryResponse struct {
	Command          string               `json:"command"`
	Status           string               `json:"status"`
	ErrorDescription string               `json:"error_description"`
	Nodes            []*api.InventoryNode `json:"nodes"`
	NodeCount        int                  `json:"node_count"`
}

func getInventoryNodes() (*inventoryResponse, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()

	response := &inventoryResponse{
		Command: "node-inventory",
		Status:  "success",
	}

	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		response.Status = "failure"
		response.ErrorDescription = "could not establish client connection"

		return response, err
	}

	nodeInventoryResponse, err := connection.NodeInventory(ctx, &api.NodeInventoryRequest{})
	if err != nil {
		response.Status = "failure"
		response.ErrorDescription = "could not retrieve inventory nodes. " + err.Error()
		return response, status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to get node inventory failed",
		)
	}

	response.Nodes = nodeInventoryResponse.Nodes
	response.NodeCount = len(nodeInventoryResponse.Nodes)

	return response, nil
}
