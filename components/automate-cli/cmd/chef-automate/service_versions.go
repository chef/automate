// Copyright © 2018 Chef Software

package main

import (
	"context"

	"github.com/spf13/cobra"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
)

var serviceVersionsCmd = &cobra.Command{
	Use:   "service-versions",
	Short: "Retrieve the versions of the individual Chef Automate services",
	Long:  "Retrieve the versions of the individual Chef Automate services",
	RunE:  runServiceVersionsCmd,
}

func runServiceVersionsCmd(cmd *cobra.Command, args []string) error {
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

func init() {
	RootCmd.AddCommand(serviceVersionsCmd)
}
