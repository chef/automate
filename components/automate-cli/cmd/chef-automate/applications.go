package main

import (
	"context"
	"fmt"
	"os"

	"github.com/golang/protobuf/jsonpb"
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

var appsCmdFlags = struct {
	thresholdMinutes int64
	format           string
	yes              bool
}{}

func init() {
	appsSubcmd := newApplicationsRootSubcmd()

	appsSubcmd.AddCommand(newApplicationsEnableCmd())
	appsSubcmd.AddCommand(newApplicationsDisableCmd())
	appsSubcmd.AddCommand(newApplicationsListDisconnectedServicesCmd())
	appsSubcmd.AddCommand(newApplicationsDeleteDisconnectedServicesCmd())

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
	c := &cobra.Command{
		Use:   "list-disconnected-services",
		Short: "List services that haven't received events from a period of time",
		RunE:  runApplicationsListDisconnectedServicesCmd,
		Args:  cobra.NoArgs,
	}
	c.PersistentFlags().Int64VarP(&appsCmdFlags.thresholdMinutes, "threshold-minutes", "m", 10, "Number of minutes since last event received")
	c.PersistentFlags().StringVarP(&appsCmdFlags.format, "format", "f", "pretty", "Format to display data. [ json | pretty ]")
	return c
}

func newApplicationsDeleteDisconnectedServicesCmd() *cobra.Command {
	c := &cobra.Command{
		Use:   "delete-disconnected-services",
		Short: "Delete services that haven't received events from a period of time",
		RunE:  runApplicationsDeleteDisconnectedServicesCmd,
		Args:  cobra.NoArgs,
	}
	c.PersistentFlags().Int64VarP(&appsCmdFlags.thresholdMinutes, "threshold-minutes", "m", 10, "Number of minutes since last event received")
	c.PersistentFlags().StringVarP(&appsCmdFlags.format, "format", "f", "pretty", "Format to display data. [ json | pretty ]")
	c.PersistentFlags().BoolVarP(&appsCmdFlags.yes, "yes", "y", false, "Delete services without a confirmation prompt.")
	return c
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
	if appsCmdFlags.thresholdMinutes <= 0 {
		return status.Errorf(status.InvalidCommandArgsError,
			"%d is not a valid threshold time in minutes. The expected time must be greater than zero.",
			appsCmdFlags.thresholdMinutes,
		)
	}

	if appsCmdFlags.format != "json" && appsCmdFlags.format != "pretty" {
		return status.Errorf(status.InvalidCommandArgsError,
			"%s is not a valid format type. Available formats are 'json' and 'pretty'.",
			appsCmdFlags.format,
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
			ThresholdSeconds: int32(appsCmdFlags.thresholdMinutes * 60),
		},
	)
	if err != nil {
		return status.Wrap(err, status.APIError, "failed to get list of disconnected services from the server")
	}

	switch appsCmdFlags.format {
	case "json":
		json, err := (&jsonpb.Marshaler{
			EmitDefaults: true,
			OrigName:     true,
		}).MarshalToString(servicesRes)
		if err != nil {
			return status.Wrap(err, status.MarshalError, "failed to convert proto into json format")
		}
		writer.Println(json)
	case "pretty":
		if len(servicesRes.Services) == 0 {
			writer.Printf(
				"There are no disconnected services with a threshold of %d minute(s)\n",
				appsCmdFlags.thresholdMinutes,
			)
			return nil
		}
		err := (&proto.TextMarshaler{}).Marshal(os.Stdout, servicesRes)
		if err != nil {
			return status.Wrap(err, status.MarshalError, "failed to convert proto into pretty format")
		}
	}

	return nil
}

func runApplicationsDeleteDisconnectedServicesCmd(*cobra.Command, []string) error {
	if appsCmdFlags.thresholdMinutes <= 0 {
		return status.Errorf(status.InvalidCommandArgsError,
			"%d is not a valid threshold time in minutes. The expected time must be greater than zero.",
			appsCmdFlags.thresholdMinutes,
		)
	}

	if appsCmdFlags.format != "json" && appsCmdFlags.format != "pretty" {
		return status.Errorf(status.InvalidCommandArgsError,
			"%s is not a valid format type. Available formats are 'json' and 'pretty'.",
			appsCmdFlags.format,
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
	req := &applications.DisconnectedServicesReq{
		ThresholdSeconds: int32(appsCmdFlags.thresholdMinutes * 60),
	}

	listRes, err := apiClient.ApplicationsClient().GetDisconnectedServices(ctx, req)
	if err != nil {
		return status.Wrap(err, status.APIError, "failed to get list of disconnected services from the server")
	}

	if len(listRes.Services) <= 0 {
		// Print this to stderr, if the user is trying to parse stdout (e.g., as
		// JSON), we don't want a text string there.
		writer.Errorf(
			"There are no disconnected services with a threshold of %d minute(s)\n",
			appsCmdFlags.thresholdMinutes,
		)
		return nil
	}
	if !appsCmdFlags.yes {
		confirmMsg := fmt.Sprintf("%d services will be deleted. Do you wish to delete them?", len(listRes.Services))
		wantToContinue, err := writer.Confirm(confirmMsg)
		if err != nil {
			return status.Wrap(err, status.UnknownError, "failed to confirm deletion of services")
		}
		if !wantToContinue {
			writer.Println("Canceled. Not deleting anything.")
			return nil
		}
	}
	deleteRes, err := apiClient.ApplicationsClient().DeleteDisconnectedServices(ctx, req)

	if err != nil {
		return status.Wrap(err, status.APIError, "request to delete disconnected services failed")
	}

	switch appsCmdFlags.format {
	case "json":
		json, err := (&jsonpb.Marshaler{
			EmitDefaults: true,
			OrigName:     true,
		}).MarshalToString(deleteRes)
		if err != nil {
			return status.Wrap(err, status.MarshalError, "failed to convert proto into json format")
		}
		writer.Println(json)
	case "pretty":
		err := (&proto.TextMarshaler{}).Marshal(os.Stdout, deleteRes)
		if err != nil {
			return status.Wrap(err, status.MarshalError, "failed to convert proto into pretty format")
		}
	}

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
