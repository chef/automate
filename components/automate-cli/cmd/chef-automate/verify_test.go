package main

import (
	"encoding/json"
	"errors"
	"net/http"
	"os"
	"strings"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifysystemdcreate"
	"github.com/chef/automate/lib/config"
	"github.com/chef/automate/lib/httputils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/chef/automate/lib/sshutils"
	"github.com/stretchr/testify/assert"
)

const (
	CONFIG_FILE         = "/config_valid_config_parser.toml"
	STATUS_API_RESPONSE = `{"status":"SUCCESS","result":{"status":"OK","services":[],"cli_version":"20230622174936","error":"error getting services from hab svc status"}}`
)

func TestRunVerifyCmd(t *testing.T) {
	tests := []struct {
		description              string
		mockHttputils            *httputils.MockHTTPClient
		mockCreateSystemdService *verifysystemdcreate.MockCreateSystemdService
		mockSystemdCreateUtils   *verifysystemdcreate.MockSystemdCreateUtils
		mockSSHUtil              *sshutils.MockSSHUtilsImpl
		configFile               string
		wantErr                  error
	}{
		{
			description: "bastion with existing automate-verify - success",
			mockHttputils: &httputils.MockHTTPClient{
				MakeRequestFunc: func(requestMethod, url string, body interface{}) (*http.Response, []byte, error) {
					if strings.Contains(url, "batch-check") {
						return makeRequestResponse(body)
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
			configFile: CONFIG_TOML_PATH + CONFIG_FILE,
			wantErr:    nil,
		},
		{
			description: "bastion without automate-verify - success",
			mockHttputils: &httputils.MockHTTPClient{
				MakeRequestFunc: func(requestMethod, url string, body interface{}) (*http.Response, []byte, error) {
					if strings.Contains(url, "batch-check") {
						return makeRequestResponse(body)
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
			configFile: CONFIG_TOML_PATH + CONFIG_FILE,
			wantErr:    nil,
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
				cw.CliWriter)

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

func getBatchCheckJSONResponse(nodeType string) ([]byte, error) {
	jsonFile := "../../pkg/verifyserver/models/testdata/"
	if nodeType == "bastion" {
		jsonFile = jsonFile + "batch-check-bastion-response.json"
	} else {
		jsonFile = jsonFile + "batch-check-remote-response.json"
	}
	return os.ReadFile(jsonFile)
}

func makeRequestResponse(body interface{}) (*http.Response, []byte, error) {
	requestBody, err := json.Marshal(body)
	if err != nil {
		return nil, nil, err
	}
	// If the request body contains system-resources check, return remote batch-check API response
	if strings.Contains(string(requestBody), "system-resources") {
		res, err := getBatchCheckJSONResponse("remote")
		if err != nil {
			return nil, nil, err
		}
		return &http.Response{
			StatusCode: http.StatusOK,
			Body:       nil,
		}, res, nil
	}
	res, err := getBatchCheckJSONResponse("bastion")
	if err != nil {
		return nil, nil, err
	}
	return &http.Response{
		StatusCode: http.StatusOK,
		Body:       nil,
	}, res, nil
}
