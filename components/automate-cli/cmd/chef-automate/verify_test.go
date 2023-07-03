package main

import (
	"errors"
	"net/http"
	"strings"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifysystemdcreate"
	"github.com/chef/automate/lib/config"
	"github.com/chef/automate/lib/httputils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/chef/automate/lib/reporting"
	"github.com/chef/automate/lib/sshutils"
	"github.com/chef/automate/lib/version"
	"github.com/gofiber/fiber/v2"
	"github.com/stretchr/testify/assert"
)

const (
	CONFIG_FILE         = "/config_valid_config_parser.toml"
	STATUS_API_RESPONSE = `{"status":"SUCCESS","result":{"status":"OK","services":[],"cli_version":"20230622174936","error":"error getting services from hab svc status"}}`
	BATCH_CHECK_REQUEST = `{"status":"SUCCESS","result":{"passed":true,"node_result":[]}}`
	AWS_CONFIG_FILE     = "/valid_config.toml"
	DARWIN              = "darwin"
)

var AwsAutoTfvarsJsonStringEmpty = `{}`

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
		IsAws                    bool
		ConvTfvarToJsonFunc      func(string) string
	}{
		{
			description: "bastion with aws automate-verify - success",
			IsAws:       true,
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
			configFile: CONFIG_AWS_TOML_PATH + AWS_CONFIG_FILE,
			wantErr:    nil,
			ConvTfvarToJsonFunc: func(string) string {
				return AwsAutoTfvarsJsonStringEmpty
			},
		},
		{
			description: "bastion aws without automate-verify - success",
			IsAws:       true,
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
			configFile: CONFIG_AWS_TOML_PATH + AWS_CONFIG_FILE,
			wantErr:    nil,
			ConvTfvarToJsonFunc: func(string) string {
				return AwsAutoTfvarsJsonStringEmpty
			},
		},
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
			ConvTfvarToJsonFunc = tt.ConvTfvarToJsonFunc
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
	tests := []struct {
		test                string
		flagsObj            *verifyCmdFlags
		ConvTfvarToJsonFunc func(string) string
	}{
		{
			test: "Existing Infra",
			flagsObj: &verifyCmdFlags{
				config: CONFIG_TOML_PATH + CONFIG_FILE,
				debug:  true,
			},
			ConvTfvarToJsonFunc: func(string) string {
				return AwsAutoTfvarsJsonStringEmpty
			},
		}, {
			test: "Aws Infra",
			flagsObj: &verifyCmdFlags{
				config: CONFIG_AWS_TOML_PATH + AWS_CONFIG_FILE,
				debug:  true,
			},
			ConvTfvarToJsonFunc: func(string) string {
				return AwsAutoTfvarsJsonStringEmpty
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.test, func(t *testing.T) {
			ConvTfvarToJsonFunc = tt.ConvTfvarToJsonFunc
			vf := verifyCmdFunc(tt.flagsObj)
			assert.NotNil(t, vf, "verifyCmdFunc should not be nil")

			err := vf(nil, nil)
			assert.Error(t, err, "expected an error")
			assert.Contains(t, err.Error(), "Cannot create automate-verify service", "unexpected error message")
		})
	}
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

func TestBuildReports(t *testing.T) {
	type args struct {
		batchCheckResults []models.BatchCheckResult
	}
	tests := []struct {
		name string
		args args
		want []reporting.VerificationReport
	}{
		{
			name: "If Report is successfully created",
			args: args{
				batchCheckResults: []models.BatchCheckResult{
					{
						NodeType: "automate",
						Ip:       "1.1.1.1",
						Tests: []models.ApiResult{
							{
								Passed:  true,
								Message: "SSH User Access Check",
								Check:   "ssh-user",
								Checks: []models.Checks{
									{
										Title:         "SSH user accessible",
										Passed:        true,
										SuccessMsg:    "SSH user is accessible for the node: 1.1.1.1",
										ErrorMsg:      "",
										ResolutionMsg: "",
									},
									{
										Title:         "Sudo password valid",
										Passed:        true,
										SuccessMsg:    "SSH user sudo password is valid for the node: 1.1.1.1",
										ErrorMsg:      "",
										ResolutionMsg: "",
									},
								},
								Skipped: false,
							},
						},
					},
				},
			},
			want: []reporting.VerificationReport{
				{
					TableKey: "automate",
					Report: reporting.Info{
						Hostip:    "1.1.1.1",
						Parameter: "ssh-user",
						Status:    "Success",
						StatusMessage: &reporting.StatusMessage{
							MainMessage: "SSH User Access Check - Success",
							SubMessage:  nil,
						},
						SummaryInfo: &reporting.SummaryInfo{
							SuccessfulCount: 2,
							FailedCount:     0,
							ToResolve:       nil,
						},
					},
				},
			},
		},
		{
			name: "Error was there form the handler or Trigger response",
			args: args{
				batchCheckResults: []models.BatchCheckResult{
					{
						NodeType: "automate",
						Ip:       "1.1.1.1",
						Tests: []models.ApiResult{
							{
								Passed:  false,
								Message: "SSH User Access Check",
								Check:   "ssh-user",
								Checks:  nil,
								Error: &fiber.Error{
									Code:    400,
									Message: "Permissions on the ssh key file do not satisfy the requirement",
								},
								Skipped: false,
							},
						},
					},
				},
			},
			want: []reporting.VerificationReport{
				{
					TableKey: "automate",
					Report: reporting.Info{
						Hostip:    "1.1.1.1",
						Parameter: "ssh-user",
						Status:    "Failed",
						StatusMessage: &reporting.StatusMessage{
							MainMessage: "SSH User Access Check - Failed",
							SubMessage:  nil,
						},
						SummaryInfo: &reporting.SummaryInfo{
							SuccessfulCount: 0,
							FailedCount:     1,
							ToResolve:       []string{"Permissions on the ssh key file do not satisfy the requirement"},
						},
					},
				},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := buildReports(tt.args.batchCheckResults)
			assert.Equal(t, got, tt.want)

		})
	}
}
