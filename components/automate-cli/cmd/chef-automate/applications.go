package main

import (
	"context"
	"os"

	"github.com/golang/protobuf/proto"
	"github.com/spf13/cobra"

	apps "github.com/chef/automate/api/config/applications"
	"github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/api/config/event"
	eventgw "github.com/chef/automate/api/config/event_gateway"
	"github.com/chef/automate/api/config/gateway"
	w "github.com/chef/automate/api/config/shared/wrappers"
	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/components/automate-cli/pkg/client/apiclient"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
)

var thresholdMinutes int64

func init() {
	appsSubcmd := newApplicationsRootSubcmd()
	appsSubcmd.AddCommand(newApplicationsEnableCmd())
	appsSubcmd.AddCommand(newApplicationsDisableCmd())

	listDisconnectedServicesCmd := newApplicationsListDisconnectedServicesCmd()
	listDisconnectedServicesCmd.PersistentFlags().Int64VarP(&thresholdMinutes, "threshold-minutes", "m", 10, "Number of minutes since last event received")
	appsSubcmd.AddCommand(listDisconnectedServicesCmd)

	RootCmd.AddCommand(appsSubcmd)
}

func newApplicationsRootSubcmd() *cobra.Command {
	return &cobra.Command{
		Use:    "applications COMMAND",
		Short:  "Manage applications visibility features",
		Hidden: true,
	}
}

func newApplicationsListDisconnectedServicesCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "list-disconnected-services",
		Short: "List services that haven't received events from a period of time",
		RunE:  runApplicationsListDisconnectedServicesCmd,
		Args:  cobra.NoArgs,
	}
}

func newApplicationsEnableCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "enable",
		Short: "Enable applications visibility features",
		RunE:  runApplicationsEnableCmd,
		Args:  cobra.NoArgs,
	}
}

func newApplicationsDisableCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "disable",
		Short: "Disable applications visibility features",
		RunE:  runApplicationsDisableCmd,
		Args:  cobra.NoArgs,
	}
}

func runApplicationsListDisconnectedServicesCmd(*cobra.Command, []string) error {
	if thresholdMinutes <= 0 {
		return status.Errorf(status.InvalidCommandArgsError,
			"%d is not a valid threshold time in minutes. The expected time must be greater than zero.",
			thresholdMinutes,
		)
	}

	var (
		ctx            = context.Background()
		apiClient, err = apiclient.OpenConnection(ctx)
	)
	if err != nil {
		return status.Wrap(err, status.APIUnreachableError,
			"Failed to create a connection to the API")
	}

	servicesRes, err := apiClient.ApplicationsClient().GetDisconnectedServices(ctx,
		&applications.DisconnectedServicesReq{
			ThresholdMinutes: int32(thresholdMinutes),
		},
	)
	if err != nil {
		return err
	}

	txt := proto.TextMarshaler{}
	txt.Marshal(os.Stdout, servicesRes)

	return nil

}

func runApplicationsEnableCmd(*cobra.Command, []string) error {
	cfg := newApplicationsToggleConfig(true)
	if err := client.PatchAutomateConfig(configCmdFlags.timeout, cfg, writer); err != nil {
		return err
	}
	return nil
}

func runApplicationsDisableCmd(*cobra.Command, []string) error {
	cfg := newApplicationsToggleConfig(false)
	if err := client.PatchAutomateConfig(configCmdFlags.timeout, cfg, writer); err != nil {
		return err
	}
	return nil
}

// newApplicationsToggleConfig creates an AutomateConfig data structure
// equivalent to the following Automate config toml, with `$ENABLE` set
// according to the `enable` argument:
// # Gateway service configuration.
// [gateway.v1]
//   [gateway.v1.sys]
//     [gateway.v1.sys.service]
//       enable_apps_feature = $ENABLE
// # event-service configuration.
// [event_service.v1]
//   [event_service.v1.sys]
//     [event_service.v1.sys.service]
//       enable_nats_feature = $ENABLE
// # event-gateway
// [event_gateway]
//   [event_gateway.v1]
//     [event_gateway.v1.sys]
//       [event_gateway.v1.sys.service]
//         enable_nats_feature = $ENABLE
// # applications-service
// [applications]
//   [applications.v1]
//     [applications.v1.sys]
//       [applications.v1.sys.service]
//         enable_nats_feature = $ENABLE
func newApplicationsToggleConfig(enable bool) *deployment.AutomateConfig {
	return &deployment.AutomateConfig{
		Gateway: &gateway.ConfigRequest{
			V1: &gateway.ConfigRequest_V1{
				Sys: &gateway.ConfigRequest_V1_System{
					Service: &gateway.ConfigRequest_V1_System_Service{
						EnableAppsFeature: w.Bool(enable),
					},
				},
			},
		},
		EventService: &event.ConfigRequest{
			V1: &event.ConfigRequest_V1{
				Sys: &event.ConfigRequest_V1_System{
					Service: &event.ConfigRequest_V1_System_Service{
						EnableNatsFeature: w.Bool(enable),
					},
				},
			},
		},
		EventGateway: &eventgw.ConfigRequest{
			V1: &eventgw.ConfigRequest_V1{
				Sys: &eventgw.ConfigRequest_V1_System{
					Service: &eventgw.ConfigRequest_V1_System_Service{
						EnableNatsFeature: w.Bool(enable),
					},
				},
			},
		},
		Applications: &apps.ConfigRequest{
			V1: &apps.ConfigRequest_V1{
				Sys: &apps.ConfigRequest_V1_System{
					Service: &apps.ConfigRequest_V1_System_Service{
						EnableNatsFeature: w.Bool(enable),
					},
				},
			},
		},
	}
}
