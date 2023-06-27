package main

import (
	"errors"
	"net/http"
	"strings"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifysystemdcreate"
	"github.com/chef/automate/lib/config"
	"github.com/chef/automate/lib/httputils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/chef/automate/lib/sshutils"
	"github.com/chef/automate/lib/version"
	"github.com/stretchr/testify/assert"
)

const (
	CONFIG_FILE         = "/config_valid_config_parser.toml"
	STATUS_API_RESPONSE = `{"status":"SUCCESS","result":{"status":"OK","services":[],"cli_version":"20230622174936","error":"error getting services from hab svc status"}}`
	BATCH_CHECK_REQUEST = `{"status":"SUCCESS","result":{"passed":true,"node_result":[]}}`
)

func TestRunVerifyCmd(t *testing.T) {
	tests := []struct {
		description              string
		mockHttputils            *httputils.MockHTTPClient
		mockCreateSystemdService *verifysystemdcreate.MockCreateSystemdService
		mockSystemdCreateUtils   *verifysystemdcreate.MockSystemdCreateUtils
		mockSSHUtil              *sshutils.MockSSHUtilsImpl
		mockVerifyCmdDeps        *verifyCmdDeps
		configFile               string
		wantErr                  error
	}{
		{
			description: "bastion with existing automate-verify - success",
			mockHttputils: &httputils.MockHTTPClient{
				MakeRequestFunc: func(requestMethod, url string, body interface{}) (*http.Response, []byte, error) {
					if strings.Contains(url, "batch-check") {
						return &http.Response{
							StatusCode: http.StatusOK,
							Body:       nil,
						}, []byte(BATCH_CHECK_REQUEST), nil
					}
					return &http.Response{
						StatusCode: http.StatusOK,
						Body:       nil,
					}, []byte(STATUS_API_RESPONSE), nil
				},
			},
			mockCreateSystemdService: &verifysystemdcreate.MockCreateSystemdService{
				CreateFun: func() error {
					return nil
				},
			},
			mockSystemdCreateUtils: &verifysystemdcreate.MockSystemdCreateUtils{
				GetBinaryPathFunc: func() (string, error) {
					return "", nil
				},
			},
			mockSSHUtil: &sshutils.MockSSHUtilsImpl{
				ExecuteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, cmd string, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
				CopyFileToRemoteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, srcFilePath string, destFileName string, removeFile bool, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
			},
			mockVerifyCmdDeps: &verifyCmdDeps{
				getAutomateHAInfraDetails: func() (*AutomateHAInfraDetails, error) {
					return &AutomateHAInfraDetails{}, nil
				},
				PopulateHaCommonConfig: func(configPuller PullConfigs) (haDeployConfig *config.HaDeployConfig, err error) {
					return &config.HaDeployConfig{}, nil
				},
			},
			configFile: CONFIG_TOML_PATH + CONFIG_FILE,
			wantErr:    nil,
		},
		{
			description: "bastion without automate-verify - success",
			mockHttputils: &httputils.MockHTTPClient{
				MakeRequestFunc: func(requestMethod, url string, body interface{}) (*http.Response, []byte, error) {
					if strings.Contains(url, "batch-check") {
						return &http.Response{
							StatusCode: http.StatusOK,
							Body:       nil,
						}, []byte(BATCH_CHECK_REQUEST), nil
					}
					return nil, nil, errors.New("some error occurred")
				},
			},
			mockCreateSystemdService: &verifysystemdcreate.MockCreateSystemdService{
				CreateFun: func() error {
					return nil
				},
			},
			mockSystemdCreateUtils: &verifysystemdcreate.MockSystemdCreateUtils{
				GetBinaryPathFunc: func() (string, error) {
					return "", nil
				},
			},
			mockSSHUtil: &sshutils.MockSSHUtilsImpl{
				ExecuteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, cmd string, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
				CopyFileToRemoteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, srcFilePath string, destFileName string, removeFile bool, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
			},
			mockVerifyCmdDeps: &verifyCmdDeps{
				getAutomateHAInfraDetails: func() (*AutomateHAInfraDetails, error) {
					return &AutomateHAInfraDetails{}, nil
				},
				PopulateHaCommonConfig: func(configPuller PullConfigs) (haDeployConfig *config.HaDeployConfig, err error) {
					return &config.HaDeployConfig{}, nil
				},
			},
			configFile: CONFIG_TOML_PATH + CONFIG_FILE,
			wantErr:    nil,
		},
		{
			description: "Failed to get automate HA infra details",
			mockHttputils: &httputils.MockHTTPClient{
				MakeRequestFunc: func(requestMethod, url string, body interface{}) (*http.Response, []byte, error) {
					if strings.Contains(url, "batch-check") {
						return &http.Response{
							StatusCode: http.StatusOK,
							Body:       nil,
						}, []byte(BATCH_CHECK_REQUEST), nil
					}
					return &http.Response{
						StatusCode: http.StatusOK,
						Body:       nil,
					}, []byte(STATUS_API_RESPONSE), nil
				},
			},
			mockCreateSystemdService: &verifysystemdcreate.MockCreateSystemdService{
				CreateFun: func() error {
					return nil
				},
			},
			mockSystemdCreateUtils: &verifysystemdcreate.MockSystemdCreateUtils{
				GetBinaryPathFunc: func() (string, error) {
					return "", nil
				},
			},
			mockSSHUtil: &sshutils.MockSSHUtilsImpl{
				ExecuteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, cmd string, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
				CopyFileToRemoteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, srcFilePath string, destFileName string, removeFile bool, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
			},
			mockVerifyCmdDeps: &verifyCmdDeps{
				getAutomateHAInfraDetails: func() (*AutomateHAInfraDetails, error) {
					return nil, errors.New("Unable to get automate HA infra details")
				},
				PopulateHaCommonConfig: func(configPuller PullConfigs) (haDeployConfig *config.HaDeployConfig, err error) {
					return &config.HaDeployConfig{}, nil
				},
			},
			configFile: "",
			wantErr:    errors.New("Unable to get automate HA infra details"),
		},
		{
			description: "Failed to populate HA common config",
			mockHttputils: &httputils.MockHTTPClient{
				MakeRequestFunc: func(requestMethod, url string, body interface{}) (*http.Response, []byte, error) {
					if strings.Contains(url, "batch-check") {
						return &http.Response{
							StatusCode: http.StatusOK,
							Body:       nil,
						}, []byte(BATCH_CHECK_REQUEST), nil
					}
					return &http.Response{
						StatusCode: http.StatusOK,
						Body:       nil,
					}, []byte(STATUS_API_RESPONSE), nil
				},
			},
			mockCreateSystemdService: &verifysystemdcreate.MockCreateSystemdService{
				CreateFun: func() error {
					return nil
				},
			},
			mockSystemdCreateUtils: &verifysystemdcreate.MockSystemdCreateUtils{
				GetBinaryPathFunc: func() (string, error) {
					return "", nil
				},
			},
			mockSSHUtil: &sshutils.MockSSHUtilsImpl{
				ExecuteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, cmd string, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
				CopyFileToRemoteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, srcFilePath string, destFileName string, removeFile bool, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
			},
			mockVerifyCmdDeps: &verifyCmdDeps{
				getAutomateHAInfraDetails: func() (*AutomateHAInfraDetails, error) {
					return &AutomateHAInfraDetails{}, nil
				},
				PopulateHaCommonConfig: func(configPuller PullConfigs) (haDeployConfig *config.HaDeployConfig, err error) {
					return nil, errors.New("Failed to populate HA common config")
				},
			},
			configFile: "",
			wantErr:    errors.New("Failed to populate HA common config"),
		},
	}

	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			cw := majorupgrade_utils.NewCustomWriter()

			vc := NewVerifyCmdFlow(tt.mockHttputils,
				tt.mockCreateSystemdService,
				tt.mockSystemdCreateUtils,
				config.NewHaDeployConfig(),
				tt.mockSSHUtil,
				cw.CliWriter,
				tt.mockVerifyCmdDeps,
			)

			flagsObj := &verifyCmdFlags{
				config: tt.configFile,
			}

			err := vc.runVerifyCmd(nil, nil, flagsObj)
			if tt.wantErr != nil {
				assert.Error(t, err)
				assert.ErrorContains(t, err, tt.wantErr.Error())
			} else {
				assert.NoError(t, err)
			}
		})
	}

}

func TestGetHostIPsWithNoLatestCLI(t *testing.T) {
	tests := []struct {
		description           string
		mockHttputils         *httputils.MockHTTPClient
		mockHostIPs           []string
		expectedIpsListLength int
		BuildVersion          string
		wantErr               error
	}{

		{
			description: "remote with existing automate-verify - success",
			mockHttputils: &httputils.MockHTTPClient{
				MakeRequestFunc: func(requestMethod, url string, body interface{}) (*http.Response, []byte, error) {
					return &http.Response{
							StatusCode: http.StatusOK,
							Body:       nil,
						}, []byte(`{
							"status": "SUCCESS",
							"result": {
									"status": "OK",
									"services": [],
									"cli_version": "2023",
									"error": "error getting services from hab svc status"
							}
					}`), nil
				},
			},
			mockHostIPs:           []string{"10.0.0.1", "10.0.0.2", "10.0.0.3", "10.0.0.4", "10.0.0.5"},
			expectedIpsListLength: 0,
			BuildVersion:          "2023",
			wantErr:               nil,
		},
		{
			description: "remote with some unreachable IP",
			mockHttputils: &httputils.MockHTTPClient{
				MakeRequestFunc: func(requestMethod, url string, body interface{}) (*http.Response, []byte, error) {
					if strings.Contains(url, "10.0.0.2") {
						return nil, nil, errors.New("failed to make HTTP request")
					}
					return &http.Response{
							StatusCode: http.StatusOK,
							Body:       nil,
						}, []byte(`{
							"status": "SUCCESS",
							"result": {
									"status": "OK",
									"services": [],
									"cli_version": "2023",
									"error": "error getting services from hab svc status"
							}
					}`), nil
				},
			},
			mockHostIPs:           []string{"10.0.0.1", "10.0.0.2", "10.0.0.3", "10.0.0.4", "10.0.0.5"},
			expectedIpsListLength: 1,
			BuildVersion:          "2023",
			wantErr:               nil,
		},
		{
			description: "remote with some of the machines have old cli version",
			mockHttputils: &httputils.MockHTTPClient{
				MakeRequestFunc: func(requestMethod, url string, body interface{}) (*http.Response, []byte, error) {
					if strings.Contains(url, "10.0.0.2") {
						return &http.Response{
								StatusCode: http.StatusOK,
							}, []byte(`{
							"status": "SUCCESS",
							"result": {
									"status": "OK",
									"services": [],
									"cli_version": "2022",
									"error": "error getting services from hab svc status"
							}
					}`), nil
					}
					return &http.Response{
							StatusCode: http.StatusOK,
						}, []byte(`{
							"status": "SUCCESS",
							"result": {
									"status": "OK",
									"services": [],
									"cli_version": "2023",
									"error": "error getting services from hab svc status"
							}
					}`), nil
				},
			},
			mockHostIPs:           []string{"10.0.0.1", "10.0.0.2", "10.0.0.3", "10.0.0.4", "10.0.0.5"},
			expectedIpsListLength: 1,
			BuildVersion:          "2023",
			wantErr:               nil,
		},
	}

	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			version.BuildTime = tt.BuildVersion
			cw := majorupgrade_utils.NewCustomWriter()

			vc := newMockVerifyCmdFlow(tt.mockHttputils, cw)

			hostIpsList, err := vc.getHostIPsWithNoLatestCLI(tt.mockHostIPs)
			if tt.wantErr != nil {
				assert.Error(t, err)
				assert.ErrorContains(t, err, tt.wantErr.Error())
			} else {
				assert.NoError(t, err)
				assert.Equal(t, tt.expectedIpsListLength, len(hostIpsList))
			}
		})
	}
}

func TestVerifyCmdFunc(t *testing.T) {
	flagsObj := &verifyCmdFlags{
		config: CONFIG_TOML_PATH + CONFIG_FILE,
		debug:  true,
	}

	vf := verifyCmdFunc(flagsObj)
	assert.NotNil(t, vf)

	err := vf(nil, nil)
	assert.Error(t, err)
	assert.ErrorContains(t, err, "Cannot create automate-verify service since systemd is not present on this machine")
}

func newMockVerifyCmdFlow(mockHttputils *httputils.MockHTTPClient, cw *majorupgrade_utils.CustomWriter) *verifyCmdFlow {

	mockCreateSystemdService := &verifysystemdcreate.MockCreateSystemdService{
		CreateFun: func() error {
			return nil
		},
	}
	mockSystemdCreateUtils := &verifysystemdcreate.MockSystemdCreateUtils{
		GetBinaryPathFunc: func() (string, error) {
			return "", nil
		},
	}
	mockSSHUtil := &sshutils.MockSSHUtilsImpl{
		ExecuteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, cmd string, hostIPs []string) []sshutils.Result {
			return []sshutils.Result{
				{
					HostIP: "",
					Error:  nil,
					Output: "",
				},
			}
		},
		CopyFileToRemoteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, srcFilePath string, destFileName string, removeFile bool, hostIPs []string) []sshutils.Result {
			return []sshutils.Result{
				{
					HostIP: "",
					Error:  nil,
					Output: "",
				},
			}
		},
	}
	mockVerifyCmdDeps := &verifyCmdDeps{
		getAutomateHAInfraDetails: func() (*AutomateHAInfraDetails, error) {
			return &AutomateHAInfraDetails{}, nil
		},
		PopulateHaCommonConfig: func(configPuller PullConfigs) (haDeployConfig *config.HaDeployConfig, err error) {
			return &config.HaDeployConfig{}, nil
		},
	}
	return NewVerifyCmdFlow(mockHttputils,
		mockCreateSystemdService,
		mockSystemdCreateUtils,
		config.NewHaDeployConfig(),
		mockSSHUtil,
		cw.CliWriter,
		mockVerifyCmdDeps,
	)
}
