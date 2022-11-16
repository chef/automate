package server

import (
	"testing"

	"github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/api/config/shared"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/protobuf/types/known/wrapperspb"
)

const (
	filePath1 = "/var/log/automate.log"
	filePath2 = "/var/tmp/automate.log"
)

func TestUpdateByMergingStructs(t *testing.T) {
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
		{
			name: "If exsting value isn't set",
			args: args{
				req: &api.PatchAutomateConfigRequest{
					Config: &deployment.AutomateConfig{
						Global: &shared.GlobalConfig{
							V1: &shared.V1{
								Log: &shared.Log{
									RedirectSysLog:       &wrapperspb.BoolValue{Value: true},
									RedirectLogFilePath:  &wrapperspb.StringValue{Value: filePath2},
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
								RedirectLogFilePath:  &wrapperspb.StringValue{Value: filePath2},
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
									RedirectLogFilePath:  &wrapperspb.StringValue{Value: filePath1},
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
								RedirectLogFilePath: &wrapperspb.StringValue{Value: filePath2},
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
								RedirectLogFilePath:  &wrapperspb.StringValue{Value: filePath1},
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
									RedirectLogFilePath: &wrapperspb.StringValue{Value: filePath2},
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
								RedirectLogFilePath:  &wrapperspb.StringValue{Value: filePath1},
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
								RedirectLogFilePath:  &wrapperspb.StringValue{Value: filePath2},
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
			got, err := UpdateByMergingStructs(tt.args.req, tt.args.existingCopy)
			require.NoError(t, err)
			require.False(t, tt.wantErr)

			require.Equal(t, got.Config.Global.V1.Log.RedirectLogFilePath.Value, tt.want.Config.Global.V1.Log.RedirectLogFilePath.Value)
			require.Equal(t, got.Config.Global.V1.Log.CompressRotatedLogs.Value, tt.want.Config.Global.V1.Log.CompressRotatedLogs.Value)
			require.Equal(t, got.Config.Global.V1.Log.MaxSizeRotateLogs.Value, tt.want.Config.Global.V1.Log.MaxSizeRotateLogs.Value)
			require.Equal(t, got.Config.Global.V1.Log.MaxNumberRotatedLogs.Value, tt.want.Config.Global.V1.Log.MaxNumberRotatedLogs.Value)
		})
	}
}

func TestSetConfigForRedirectLogs(t *testing.T) {
	type args struct {
		req          *api.PatchAutomateConfigRequest
		existingCopy *deployment.AutomateConfig
	}
	tests := []struct {
		name string
		args args
	}{
		{
			name: "RedirectSysLog is true for patch and existing",
			args: args{
				req: &api.PatchAutomateConfigRequest{
					Config: &deployment.AutomateConfig{
						Global: &shared.GlobalConfig{
							V1: &shared.V1{
								Log: &shared.Log{
									RedirectSysLog:       &wrapperspb.BoolValue{Value: true},
									RedirectLogFilePath:  &wrapperspb.StringValue{Value: filePath2},
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
		},
		{
			name: "If merge result and existing are equal",
			args: args{
				req: &api.PatchAutomateConfigRequest{
					Config: &deployment.AutomateConfig{
						Global: &shared.GlobalConfig{
							V1: &shared.V1{
								Log: &shared.Log{
									RedirectSysLog: &wrapperspb.BoolValue{Value: true},
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
								RedirectLogFilePath:  &wrapperspb.StringValue{Value: filePath1},
								CompressRotatedLogs:  &wrapperspb.BoolValue{Value: true},
								MaxSizeRotateLogs:    &wrapperspb.StringValue{Value: "30M"},
								MaxNumberRotatedLogs: &wrapperspb.Int32Value{Value: 11},
							},
						},
					},
				},
			},
		},
		{
			name: "If requested RedirectLogFilePath is same as existing",
			args: args{
				req: &api.PatchAutomateConfigRequest{
					Config: &deployment.AutomateConfig{
						Global: &shared.GlobalConfig{
							V1: &shared.V1{
								Log: &shared.Log{
									RedirectSysLog:      &wrapperspb.BoolValue{Value: true},
									RedirectLogFilePath: &wrapperspb.StringValue{Value: filePath1},
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
								RedirectLogFilePath: &wrapperspb.StringValue{Value: filePath1},
							},
						},
					},
				},
			},
		},
	}
	{
		for _, tt := range tests {
			t.Run(tt.name, func(t *testing.T) {
				err := setConfigForRedirectLogs(tt.args.req, tt.args.existingCopy)
				if err == nil {
					assert.NoError(t, err)
				} else {
					assert.Error(t, err)
				}
			})
		}
	}

}
