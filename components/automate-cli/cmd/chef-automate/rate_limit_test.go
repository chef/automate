package main

import (
	"errors"
	"fmt"
	"os"
	"testing"

	"github.com/chef/automate/api/config/deployment"
	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/api/config/shared"
	"github.com/stretchr/testify/assert"
	"google.golang.org/protobuf/types/known/wrapperspb"
)

func TestRemoveRateLimitFile(t *testing.T) {
	respGot := removeRateLimitFile()
	expected := fmt.Sprintf("sudo rm -f %s ; \n %s; \n", journaldConfigFile, restartJournaldService)
	assert.Equal(t, expected, respGot)
}

func TestCreateConfigFileForJournald(t *testing.T) {
	testCases := []struct {
		name              string
		rateLimitBurst    int32
		rateLimitInterval int32
		expectedOutput    string
	}{
		{
			name:              "Giving Normal Values",
			rateLimitBurst:    1000,
			rateLimitInterval: 10,
			expectedOutput:    fmt.Sprintf("[Journal]\nRateLimitBurst=%d\nRateLimitInterval=%dms\n", 1000, 10),
		},
		{
			name:              "Giving Large Values",
			rateLimitBurst:    10000000,
			rateLimitInterval: 10000000,
			expectedOutput:    fmt.Sprintf("[Journal]\nRateLimitBurst=%d\nRateLimitInterval=%dms\n", 10000000, 10000000),
		},
	}
	for _, tt := range testCases {
		t.Run(tt.name, func(t *testing.T) {
			respGot := createConfigFileForJournald(tt.rateLimitBurst, tt.rateLimitInterval)
			assert.Equal(t, tt.expectedOutput, respGot)
		})
	}
}

func TestCreateScriptCommandsForRateLimit(t *testing.T) {
	testCases := []struct {
		name           string
		reqConfig      *dc.AutomateConfig
		expectedOutput string
	}{
		{
			name: "Config contains both RateLimitBurst and RateInterval values",
			reqConfig: &dc.AutomateConfig{
				Global: &shared.GlobalConfig{
					V1: &shared.V1{
						Log: &shared.Log{
							RateLimitInterval: &wrapperspb.Int32Value{
								Value: 1000,
							},
							RateLimitBurst: &wrapperspb.Int32Value{
								Value: 1000,
							},
						},
					},
				},
			},
			expectedOutput: fmt.Sprintf("sudo mkdir -p /etc/systemd/journald.conf.d ; \n sudo sh -c 'echo \"[Journal]\nRateLimitBurst=%d\nRateLimitInterval=%dms\n\" > /etc/systemd/journald.conf.d/automate.conf'; \n sudo systemctl restart systemd-journald.service;", 1000, 1000),
		},
		{
			name: "Config have only RateLimitBurst value",
			reqConfig: &dc.AutomateConfig{
				Global: &shared.GlobalConfig{
					V1: &shared.V1{
						Log: &shared.Log{
							RateLimitBurst: &wrapperspb.Int32Value{
								Value: 1000,
							},
						},
					},
				},
			},
			expectedOutput: fmt.Sprintf("sudo mkdir -p /etc/systemd/journald.conf.d ; \n sudo sh -c 'echo \"[Journal]\nRateLimitBurst=%d\nRateLimitInterval=%dms\n\" > /etc/systemd/journald.conf.d/automate.conf'; \n sudo systemctl restart systemd-journald.service;", 1000, defaultRateLimitIntervalJournald),
		},
		{
			name: "Config haven't any value",
			reqConfig: &dc.AutomateConfig{
				Global: &shared.GlobalConfig{
					V1: &shared.V1{
						Log: &shared.Log{},
					},
				},
			},
			expectedOutput: fmt.Sprintf("sudo mkdir -p /etc/systemd/journald.conf.d ; \n sudo sh -c 'echo \"[Journal]\nRateLimitBurst=%d\nRateLimitInterval=%dms\n\" > /etc/systemd/journald.conf.d/automate.conf'; \n sudo systemctl restart systemd-journald.service;", defaultRateLimitBurstJournald, defaultRateLimitIntervalJournald),
		},
	}
	for _, tt := range testCases {
		t.Run(tt.name, func(t *testing.T) {
			respGot := createScriptCommandsForRateLimit(tt.reqConfig)
			assert.Equal(t, tt.expectedOutput, respGot)
		})
	}
}

func TestUpdateTomlFileFromConfig(t *testing.T) {
	postgresLogConfigActualFileName := postgresLogConfig
	opensearchLogConfigActualFileName := opensearchConfig
	postgresLogConfig = "postgres_rate_limit_unit_test_file.toml"
	opensearchConfig = "opensearch_rate_limit_unit_test_file.toml"
	tests := []struct {
		name             string
		remoteType       string
		reqConfig        *deployment.AutomateConfig
		fileName         string
		expectedFileData string
		expectedErr      error
	}{
		{
			name:       "Updating postgresql config file",
			remoteType: "postgresql",
			reqConfig: &deployment.AutomateConfig{
				Global: &shared.GlobalConfig{
					V1: &shared.V1{
						Log: &shared.Log{
							RedirectSysLog: &wrapperspb.BoolValue{
								Value: true,
							},
							RedirectLogFilePath: &wrapperspb.StringValue{
								Value: "/home/ec2-user",
							},
						},
					},
				},
			},
			fileName: postgresLogConfig,
			expectedFileData: `[global]
  [global.v1]
    [global.v1.log]
      redirect_sys_log = true
      redirect_log_file_path = "/home/ec2-user"
`,
			expectedErr: nil,
		},
		{
			name:       "Updating opensearch config file",
			remoteType: "opensearch",
			reqConfig: &deployment.AutomateConfig{
				Global: &shared.GlobalConfig{
					V1: &shared.V1{
						Log: &shared.Log{
							RedirectSysLog: &wrapperspb.BoolValue{
								Value: true,
							},
							RedirectLogFilePath: &wrapperspb.StringValue{
								Value: "/home/ec2-user",
							},
						},
					},
				},
			},
			fileName: opensearchConfig,
			expectedFileData: `[global]
  [global.v1]
    [global.v1.log]
      redirect_sys_log = true
      redirect_log_file_path = "/home/ec2-user"
`,
			expectedErr: nil,
		},
		{
			name:             "Config is empty and remote type is opensearch",
			remoteType:       "opensearch",
			reqConfig:        &deployment.AutomateConfig{},
			fileName:         opensearchConfig,
			expectedFileData: "",
			expectedErr:      nil,
		},
		{
			name:             "Config is empty and remote type is unknown",
			remoteType:       "opensearch",
			reqConfig:        &deployment.AutomateConfig{},
			fileName:         opensearchConfig,
			expectedFileData: "",
			expectedErr:      nil,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			updateTomlFileFromConfig(tt.remoteType, tt.reqConfig)
			respGot, err := readFileContent(tt.fileName)
			assert.NoError(t, err)
			assert.Equal(t, tt.expectedFileData, respGot)
			err = os.Remove(tt.fileName)
			assert.NoError(t, err)
		})
	}
	postgresLogConfig = postgresLogConfigActualFileName
	opensearchConfig = opensearchLogConfigActualFileName
}

func TestGetScriptCommandsForRateLimitJournald(t *testing.T) {
	tests := []struct {
		name                   string
		reqConfig              *deployment.AutomateConfig
		existConfig            *deployment.AutomateConfig
		expectedScriptCommands string
	}{
		{
			name:                   "Both Config is empty",
			reqConfig:              &deployment.AutomateConfig{},
			existConfig:            &deployment.AutomateConfig{},
			expectedScriptCommands: "",
		},
		{
			name: "Exist Config is empty",
			reqConfig: &deployment.AutomateConfig{
				Global: &shared.GlobalConfig{
					V1: &shared.V1{
						Log: &shared.Log{
							RateLimitInterval: &wrapperspb.Int32Value{
								Value: 1000,
							},
							RateLimitBurst: &wrapperspb.Int32Value{
								Value: 1000,
							},
						},
					},
				},
			},
			existConfig:            &deployment.AutomateConfig{},
			expectedScriptCommands: fmt.Sprintf("sudo mkdir -p /etc/systemd/journald.conf.d ; \n sudo sh -c 'echo \"[Journal]\nRateLimitBurst=%d\nRateLimitInterval=%dms\n\" > /etc/systemd/journald.conf.d/automate.conf'; \n sudo systemctl restart systemd-journald.service;", 1000, 1000),
		},
		{
			name:      "Req Config is empty - Don;t need to do anything hence returning empty string",
			reqConfig: &deployment.AutomateConfig{},
			existConfig: &deployment.AutomateConfig{
				Global: &shared.GlobalConfig{
					V1: &shared.V1{
						Log: &shared.Log{
							RateLimitInterval: &wrapperspb.Int32Value{
								Value: 1000,
							},
							RateLimitBurst: &wrapperspb.Int32Value{
								Value: 1000,
							},
						},
					},
				},
			},
			expectedScriptCommands: "",
		},
		{
			name: "Both Config is Present - And user update the value through req config",
			reqConfig: &deployment.AutomateConfig{
				Global: &shared.GlobalConfig{
					V1: &shared.V1{
						Log: &shared.Log{
							RateLimitInterval: &wrapperspb.Int32Value{
								Value: 3000,
							},
						},
					},
				},
			},
			existConfig: &deployment.AutomateConfig{
				Global: &shared.GlobalConfig{
					V1: &shared.V1{
						Log: &shared.Log{
							RateLimitInterval: &wrapperspb.Int32Value{
								Value: 1000,
							},
							RateLimitBurst: &wrapperspb.Int32Value{
								Value: 2000,
							},
						},
					},
				},
			},
			expectedScriptCommands: fmt.Sprintf("sudo mkdir -p /etc/systemd/journald.conf.d ; \n sudo sh -c 'echo \"[Journal]\nRateLimitBurst=%d\nRateLimitInterval=%dms\n\" > /etc/systemd/journald.conf.d/automate.conf'; \n sudo systemctl restart systemd-journald.service;", 2000, 3000),
		},
		{
			name: "Both Config is Present - user applied the same config, hence no change therefore returning empty string",
			reqConfig: &deployment.AutomateConfig{
				Global: &shared.GlobalConfig{
					V1: &shared.V1{
						Log: &shared.Log{
							RateLimitInterval: &wrapperspb.Int32Value{
								Value: 1000,
							},
						},
					},
				},
			},
			existConfig: &deployment.AutomateConfig{
				Global: &shared.GlobalConfig{
					V1: &shared.V1{
						Log: &shared.Log{
							RateLimitInterval: &wrapperspb.Int32Value{
								Value: 1000,
							},
							RateLimitBurst: &wrapperspb.Int32Value{
								Value: 1000,
							},
						},
					},
				},
			},
			expectedScriptCommands: "",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			respGot := getScriptCommandsForRateLimitJournald(tt.reqConfig, tt.existConfig)
			assert.Equal(t, tt.expectedScriptCommands, respGot)
		})
	}
}

func TestRemoveRateLimiterConfig(t *testing.T) {
	onlyRateLimitConfigfileName := "onlyRateLimitConfig.toml"
	postgresNodeConfigFileName := "pglogLevel.toml"
	opensearchNodeConfigFileName := "oslogLevel.toml"
	err := createFileWithContent(onlyRateLimitConfigfileName, `[global.v1.log]
rate_limit_interval = 10
rate_limit_burst = 10000`)
	assert.NoError(t, err)
	err = createFileWithContent(postgresNodeConfigFileName, `log_level="debug"`)
	assert.NoError(t, err)
	err = createFileWithContent(opensearchNodeConfigFileName, `[logger]
level="debug"`)
	assert.NoError(t, err)
	tests := []struct {
		name                   string
		configFileName         string
		remoteType             string
		expectedNewFileContent string
		expectedError          error
	}{
		{
			name:                   "User Passed only RateLimiter Config for postgres Node",
			configFileName:         onlyRateLimitConfigfileName,
			remoteType:             "postgresql",
			expectedNewFileContent: "",
			expectedError:          nil,
		},
		{
			name:                   "User Passed only RateLimiter Config for Opensearch Node",
			configFileName:         onlyRateLimitConfigfileName,
			remoteType:             "opensearch",
			expectedNewFileContent: "",
			expectedError:          nil,
		},
		{
			name:                   "Used Passed Postgres Node Config only, not passed rateLimit Config",
			configFileName:         postgresNodeConfigFileName,
			remoteType:             "postgresql",
			expectedNewFileContent: "log_level = \"debug\"\n",
			expectedError:          nil,
		},
		{
			name:                   "Used Passed Opensearch Node Config only, not passed rateLimit Config",
			configFileName:         opensearchNodeConfigFileName,
			remoteType:             "opensearch",
			expectedNewFileContent: "[logger]\n  level = \"debug\"\n",
			expectedError:          nil,
		},
		{
			name:                   "File Doesn't Exist - pg node",
			configFileName:         "don't_have_permission/deletefile.toml",
			remoteType:             "postgresql",
			expectedNewFileContent: "",
			expectedError:          errors.New(""),
		},
		{
			name:                   "File Doesn't Exist - os node",
			configFileName:         "don't_have_permission/deletefile.toml",
			remoteType:             "opensearch",
			expectedNewFileContent: "",
			expectedError:          errors.New(""),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if tt.remoteType == "postgresql" {
				configCmdFlags.postgresql = true
			} else if tt.remoteType == "opensearch" {
				configCmdFlags.opensearch = true
			}
			respFileName, err := removeRateLimiterConfig([]string{tt.configFileName})
			if tt.expectedError != nil {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
				respFileContent, err := readFileContent(respFileName)
				assert.NoError(t, err)
				assert.Equal(t, tt.expectedNewFileContent, respFileContent)
				err = os.Remove(respFileName)
				assert.NoError(t, err)
			}
			if tt.remoteType == "postgresql" {
				configCmdFlags.postgresql = false
			} else if tt.remoteType == "opensearch" {
				configCmdFlags.opensearch = false
			}
		})
	}
	err = os.Remove(onlyRateLimitConfigfileName)
	assert.NoError(t, err)
	err = os.Remove(postgresNodeConfigFileName)
	assert.NoError(t, err)
	err = os.Remove(opensearchNodeConfigFileName)
	assert.NoError(t, err)
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

func createFileWithContent(filename, content string) error {
	// Create the file
	file, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer file.Close()

	// Write the content to the file
	_, err = file.WriteString(content)
	if err != nil {
		return err
	}

	return nil
}
