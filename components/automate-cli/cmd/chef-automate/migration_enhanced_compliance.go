package main

import (
	"context"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	pb "github.com/golang/protobuf/ptypes/empty"
	"github.com/spf13/cobra"
)

var enhanceComplianceCmd = &cobra.Command{
	Use:    "enhance-compliance",
	Short:  "Utilities for enhance-compliance reporting",
	Hidden: true,
}

var enhanceMigrateComplianceCmd = &cobra.Command{
	Use:    "migrate",
	Short:  "Utilities for enhance-compliance migrate which migrates compliance reports to new indexes for enhanced compliance reporting",
	Hidden: true,
}

func newMigrateStatusCmd() *cobra.Command {
	var migrateDataCmd = &cobra.Command{
		Use:   "status",
		Short: "Chef Automate enhanced-compliance migrate status",
		Long:  "Chef Automate command to find the status of the compliance report migration to new indexes for enhanced compliance reporting",
		RunE:  runEnhancedComplianceMigrateStatusCmd,
	}
	return migrateDataCmd
}

func runEnhancedComplianceMigrateStatusCmd(cmd *cobra.Command, args []string) error {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return err
	}
	response, err := connection.ControlIndexUpgradeStatus(context.Background(), &pb.Empty{})
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to call migration status failed",
		)
	}

	writer.Println(response.Status)
	return nil
}

func init() {
	RootCmd.AddCommand(enhanceComplianceCmd)
	enhanceComplianceCmd.AddCommand(enhanceMigrateComplianceCmd)
	enhanceMigrateComplianceCmd.AddCommand(newMigrateStatusCmd())
}
