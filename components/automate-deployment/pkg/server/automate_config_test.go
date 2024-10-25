package server

import (
	"os"
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

func readFileContent(filename string) (string, error) {
	// Read the entire file content as a byte slice
	content, err := os.ReadFile(filename)
	if err != nil {
		return "", err
	}

	// Convert the byte slice to a string and return it
	return string(content), nil
}

func TestCreateConfigFileForJournald(t *testing.T) {
	actualjournaldConfigFile := journaldConfigFile
	actualjournaldConfigFilePath := journaldConfigFilePath
	journaldConfigFile = "temp.conf"
	journaldConfigFilePath = "./temp"

	t.Run("User able to create configfile for journald", func(t *testing.T) {
		err := createConfigFileForJournald(1000, 1000)
		assert.NoError(t, err)
		expectedFileContent := `[Journal]
RateLimitBurst=1000
RateLimitInterval=1000ms
`
		fileContentgot, err := readFileContent(journaldConfigFile)
		assert.NoError(t, err)
		assert.Equal(t, expectedFileContent, fileContentgot)
		err = os.Remove(journaldConfigFile)
		assert.NoError(t, err)
	})

	journaldConfigFile = actualjournaldConfigFile
	journaldConfigFilePath = actualjournaldConfigFilePath
}

func TestGetRateLimitValues(t *testing.T) {
	tests := []struct {
		name                      string
		reqConfig                 *api.PatchAutomateConfigRequest
		existingConfig            *deployment.AutomateConfig
		rateLimitBurst            int32
		rateLimitInterval         int32
		expectedRateLimitBurst    int32
		expectedRateLimitInterval int32
	}{
		{
			name:                      "Both reqConfig and existingConfig is nil, hence taking default values",
			reqConfig:                 nil,
			existingConfig:            nil,
			rateLimitBurst:            100,
			rateLimitInterval:         1000,
			expectedRateLimitBurst:    100,
			expectedRateLimitInterval: 1000,
		},
		{
			name: "reqConfig is present but existingConfig is nil",
			reqConfig: &api.PatchAutomateConfigRequest{
				Config: &deployment.AutomateConfig{
					Global: &shared.GlobalConfig{
						V1: &shared.V1{
							Log: &shared.Log{
								RateLimitInterval: &wrapperspb.Int32Value{
									Value: 2000,
								},
								RateLimitBurst: &wrapperspb.Int32Value{
									Value: 2000,
								},
							},
						},
					},
				},
			},
			existingConfig:            nil,
			rateLimitBurst:            100,
			rateLimitInterval:         1000,
			expectedRateLimitBurst:    2000,
			expectedRateLimitInterval: 2000,
		},
		{
			name: "existingConfig is present but reqConfig is nil",
			existingConfig: &deployment.AutomateConfig{
				Global: &shared.GlobalConfig{
					V1: &shared.V1{
						Log: &shared.Log{
							RateLimitInterval: &wrapperspb.Int32Value{
								Value: 2000,
							},
							RateLimitBurst: &wrapperspb.Int32Value{
								Value: 2000,
							},
						},
					},
				},
			},
			reqConfig:                 nil,
			rateLimitBurst:            100,
			rateLimitInterval:         1000,
			expectedRateLimitBurst:    2000,
			expectedRateLimitInterval: 2000,
		},
		{
			name: "both reqConfig and exist config present",
			reqConfig: &api.PatchAutomateConfigRequest{
				Config: &deployment.AutomateConfig{
					Global: &shared.GlobalConfig{
						V1: &shared.V1{
							Log: &shared.Log{
								RateLimitInterval: &wrapperspb.Int32Value{
									Value: 2000,
								},
								RateLimitBurst: &wrapperspb.Int32Value{
									Value: 2000,
								},
							},
						},
					},
				},
			},
			existingConfig: &deployment.AutomateConfig{
				Global: &shared.GlobalConfig{
					V1: &shared.V1{
						Log: &shared.Log{
							RateLimitInterval: &wrapperspb.Int32Value{
								Value: 3000,
							},
							RateLimitBurst: &wrapperspb.Int32Value{
								Value: 3000,
							},
						},
					},
				},
			},
			rateLimitBurst:            100,
			rateLimitInterval:         1000,
			expectedRateLimitBurst:    2000,
			expectedRateLimitInterval: 2000,
		},
		{
			name: "config doesn't have value of rateLimitBurst. Neither in req nor in existing. Hence taking default value",
			reqConfig: &api.PatchAutomateConfigRequest{
				Config: &deployment.AutomateConfig{
					Global: &shared.GlobalConfig{
						V1: &shared.V1{
							Log: &shared.Log{
								RateLimitInterval: &wrapperspb.Int32Value{
									Value: 2000,
								},
							},
						},
					},
				},
			},
			existingConfig: &deployment.AutomateConfig{
				Global: &shared.GlobalConfig{
					V1: &shared.V1{
						Log: &shared.Log{
							RateLimitInterval: &wrapperspb.Int32Value{
								Value: 4000,
							},
						},
					},
				},
			},
			rateLimitBurst:            100,
			rateLimitInterval:         1000,
			expectedRateLimitBurst:    100,
			expectedRateLimitInterval: 2000,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			rateLimitBurstGot, rateLimitIntervalGot := getRateLimitValues(tt.reqConfig, tt.existingConfig, tt.rateLimitBurst, tt.rateLimitInterval)
			assert.Equal(t, tt.expectedRateLimitBurst, rateLimitBurstGot)
			assert.Equal(t, tt.expectedRateLimitInterval, rateLimitIntervalGot)
		})
	}
}

func TestCreateConfigFileForAutomateSysLog(t *testing.T) {
	actualRsyslogConfigFile := rsyslogConfigFile
	rsyslogConfigFile = "temp.conf"

	t.Run("User able to create configfile for automate syslog", func(t *testing.T) {
		err := createConfigFileForAutomateSysLog("LogDirectory/", 1000, 1000)
		assert.NoError(t, err)
		expectedFileContent := `$imjournalRatelimitBurst 1000
$imjournalRatelimitInterval 1000
if $programname == 'hab' then LogDirectory/automate.log
& stop
`
		fileContentgot, err := readFileContent(rsyslogConfigFile)
		assert.NoError(t, err)
		assert.Equal(t, expectedFileContent, fileContentgot)
		err = os.Remove(rsyslogConfigFile)
		assert.NoError(t, err)
	})

	rsyslogConfigFile = actualRsyslogConfigFile
}
