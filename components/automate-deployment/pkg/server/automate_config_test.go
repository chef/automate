package server

import (
	"testing"

	"github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/api/config/shared"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/sirupsen/logrus"
	"google.golang.org/protobuf/types/known/wrapperspb"
)

func TestUpdateOfLogroateConfig(t *testing.T) {
	type args struct {
		req          *api.PatchAutomateConfigRequest
		existingCopy *deployment.AutomateConfig
	}
	tests := []struct {
		name    string
		args    args
		want    *api.PatchAutomateConfigRequest
		wantErr bool
	}{
		// TODO: Add test cases.
		{
			name: "If exsting value isn't set",
			args: args{
				req: &api.PatchAutomateConfigRequest{
					Config: &deployment.AutomateConfig{
						Global: &shared.GlobalConfig{
							V1: &shared.V1{
								Log: &shared.Log{
									RedirectSysLog:       &wrapperspb.BoolValue{Value: true},
									RedirectLogFilePath:  &wrapperspb.StringValue{Value: "/var/tmp/automate.log"},
									CompressRotatedLogs:  &wrapperspb.BoolValue{Value: true},
									MaxSizeRotateLogs:    &wrapperspb.StringValue{Value: "10"},
									MaxNumberRotatedLogs: &wrapperspb.Int32Value{Value: 3},
								},
							},
						},
					},
				},
				existingCopy: &deployment.AutomateConfig{
					Global: &shared.GlobalConfig{
						V1: &shared.V1{
							Log: &shared.Log{
								RedirectSysLog: &wrapperspb.BoolValue{Value: true},
							},
						},
					},
				},
			},
			want: &api.PatchAutomateConfigRequest{
				Config: &deployment.AutomateConfig{
					Global: &shared.GlobalConfig{
						V1: &shared.V1{
							Log: &shared.Log{
								RedirectSysLog:       &wrapperspb.BoolValue{Value: true},
								RedirectLogFilePath:  &wrapperspb.StringValue{Value: "/var/tmp/automate.log"},
								CompressRotatedLogs:  &wrapperspb.BoolValue{Value: true},
								MaxSizeRotateLogs:    &wrapperspb.StringValue{Value: "10"},
								MaxNumberRotatedLogs: &wrapperspb.Int32Value{Value: 3},
							},
						},
					},
				},
			},
			wantErr: false,
		},
		{
			name: "If keys are missing in existing value",
			args: args{
				req: &api.PatchAutomateConfigRequest{
					Config: &deployment.AutomateConfig{
						Global: &shared.GlobalConfig{
							V1: &shared.V1{
								Log: &shared.Log{
									RedirectSysLog:       &wrapperspb.BoolValue{Value: true},
									RedirectLogFilePath:  &wrapperspb.StringValue{Value: "/var/var/automate.log"},
									CompressRotatedLogs:  &wrapperspb.BoolValue{Value: false},
									MaxSizeRotateLogs:    &wrapperspb.StringValue{Value: "30M"},
									MaxNumberRotatedLogs: &wrapperspb.Int32Value{Value: 11},
								},
							},
						},
					},
				},
				existingCopy: &deployment.AutomateConfig{
					Global: &shared.GlobalConfig{
						V1: &shared.V1{
							Log: &shared.Log{
								RedirectSysLog:      &wrapperspb.BoolValue{Value: true},
								RedirectLogFilePath: &wrapperspb.StringValue{Value: "/var/tmp/automate.log"},
								CompressRotatedLogs: &wrapperspb.BoolValue{Value: true},

								MaxNumberRotatedLogs: &wrapperspb.Int32Value{Value: 10},
							},
						},
					},
				},
			},
			want: &api.PatchAutomateConfigRequest{
				Config: &deployment.AutomateConfig{
					Global: &shared.GlobalConfig{
						V1: &shared.V1{
							Log: &shared.Log{
								RedirectSysLog:       &wrapperspb.BoolValue{Value: true},
								RedirectLogFilePath:  &wrapperspb.StringValue{Value: "/var/var/automate.log"},
								CompressRotatedLogs:  &wrapperspb.BoolValue{Value: true},
								MaxSizeRotateLogs:    &wrapperspb.StringValue{Value: "30M"},
								MaxNumberRotatedLogs: &wrapperspb.Int32Value{Value: 11},
							},
						},
					},
				},
			},
			wantErr: false,
		},
		{
			name: "If keys are missing in requested value",
			args: args{
				req: &api.PatchAutomateConfigRequest{
					Config: &deployment.AutomateConfig{
						Global: &shared.GlobalConfig{
							V1: &shared.V1{
								Log: &shared.Log{
									RedirectSysLog:      &wrapperspb.BoolValue{Value: true},
									RedirectLogFilePath: &wrapperspb.StringValue{Value: "/var/tmp/automate.log"},
									MaxSizeRotateLogs:   &wrapperspb.StringValue{Value: "30M"},
								},
							},
						},
					},
				},
				existingCopy: &deployment.AutomateConfig{
					Global: &shared.GlobalConfig{
						V1: &shared.V1{
							Log: &shared.Log{
								RedirectSysLog:       &wrapperspb.BoolValue{Value: true},
								RedirectLogFilePath:  &wrapperspb.StringValue{Value: "/var/var/automate.log"},
								CompressRotatedLogs:  &wrapperspb.BoolValue{Value: true},
								MaxSizeRotateLogs:    &wrapperspb.StringValue{Value: "30M"},
								MaxNumberRotatedLogs: &wrapperspb.Int32Value{Value: 11},
							},
						},
					},
				},
			},
			want: &api.PatchAutomateConfigRequest{
				Config: &deployment.AutomateConfig{
					Global: &shared.GlobalConfig{
						V1: &shared.V1{
							Log: &shared.Log{
								RedirectSysLog:       &wrapperspb.BoolValue{Value: true},
								RedirectLogFilePath:  &wrapperspb.StringValue{Value: "/var/tmp/automate.log"},
								CompressRotatedLogs:  &wrapperspb.BoolValue{Value: true},
								MaxSizeRotateLogs:    &wrapperspb.StringValue{Value: "30M"},
								MaxNumberRotatedLogs: &wrapperspb.Int32Value{Value: 11},
							},
						},
					},
				},
			},
			wantErr: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := UpdateOfLogroateConfig(tt.args.req, tt.args.existingCopy)
			logrus.Printf("Final: %+v", got)

			if (err != nil) != tt.wantErr {
				t.Errorf("editConfig() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if got.Config.Global.V1.Log.RedirectLogFilePath.Value != tt.want.Config.Global.V1.Log.RedirectLogFilePath.Value {
				t.Errorf("Values aren't the same")
			}
			if got.Config.Global.V1.Log.CompressRotatedLogs.Value != tt.want.Config.Global.V1.Log.CompressRotatedLogs.Value {
				t.Errorf("Values aren't the same")
			}
			if got.Config.Global.V1.Log.MaxSizeRotateLogs.Value != tt.want.Config.Global.V1.Log.MaxSizeRotateLogs.Value {
				t.Errorf("Values aren't the same")
			}
			if got.Config.Global.V1.Log.MaxNumberRotatedLogs.Value != tt.want.Config.Global.V1.Log.MaxNumberRotatedLogs.Value {
				t.Errorf("Values aren't the same")
			}
		})
	}
}
