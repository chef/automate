package main

import (
	"github.com/spf13/cobra"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/api/config/load_balancer"
	w "github.com/chef/automate/api/config/shared/wrappers"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
)

var maintenanceOpts = struct {
	timeout int64
}{}

func init() {
	maintenanceCmd.PersistentFlags().Int64VarP(&maintenanceOpts.timeout, "timeout", "t", 10, "Request timeout in seconds")

	RootCmd.AddCommand(maintenanceCmd)
}

var maintenanceCmd = &cobra.Command{
	Use:   "maintenance [on|off]",
	Short: "Put Chef Automate into or out of maintenance mode",
	Long:  "Chef Automate maintenance mode keeps all services running but rejects new connections at the load balancer so that maintenance operations can be performed.",
	Args:  cobra.ExactArgs(1),
	RunE:  runMaintenanceCmd,
}

func runMaintenanceCmd(cmd *cobra.Command, args []string) error {
	if args[0] != "off" && args[0] != "on" {
		return status.New(status.InvalidCommandArgsError, "you must supply either 'on' or 'off'")
	}

	enable := w.Bool(false)
	if args[0] == "on" {
		enable = w.Bool(true)
	}

	cfg := dc.NewUserOverrideConfig()
	cfg.LoadBalancer = &load_balancer.ConfigRequest{
		V1: &load_balancer.ConfigRequest_V1{
			Sys: &load_balancer.ConfigRequest_V1_System{
				Service: &load_balancer.ConfigRequest_V1_System_Service{
					MaintenanceMode: enable,
				},
			},
		},
	}

	if err := client.PatchAutomateConfig(maintenanceOpts.timeout, cfg, writer); err != nil {
		return err
	}

	return nil
}
