package main

import (
	"fmt"
	"testing"
	"time"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
)

func TestRunCommand(t *testing.T) {
	infra := &AutomteHAInfraDetails{}
	infra.Outputs.SSHUser.Value = "ubuntu"
	infra.Outputs.SSHKeyFile.Value = "new.pem"
	infra.Outputs.SSHPort.Value = "22"
	infra.Outputs.AutomatePrivateIps.Value = []string{"127.0.0.3", "127.0.0.4", "127.0.0.5"}
	infra.Outputs.ChefServerPrivateIps.Value = []string{"127.0.0.3", "127.0.0.4", "127.0.0.5"}
	infra.Outputs.PostgresqlPrivateIps.Value = []string{"127.0.0.3", "127.0.0.4", "127.0.0.5"}
	infra.Outputs.OpensearchPrivateIps.Value = []string{"127.0.0.3", "127.0.0.4", "127.0.0.5"}

	testCases := []struct {
		input   *NodeTypeAndCmd
		sshUtil SSHUtil
		isError bool
		err     error
	}{
		{
			input: &NodeTypeAndCmd{
				Frontend: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:      "sudo chef-automate config show",
						Single:   false,
						NodeType: true,
					},
				},
				Automate: &Cmd{
					CmdInputs: &CmdInputs{
						NodeType: false,
					},
				},
				Chef_server: &Cmd{
					CmdInputs: &CmdInputs{
						NodeType: false,
					}},
				Postgresql: &Cmd{
					CmdInputs: &CmdInputs{
						NodeType: false,
					}},
				Opensearch: &Cmd{
					CmdInputs: &CmdInputs{
						NodeType: false,
					}},
				Infra: infra,
			},
			sshUtil: GetMockSSHUtil(&SSHConfig{}, nil, "config show operation completed", nil, "", nil),
			isError: false,
			err:     nil,
		},
		{
			input: &NodeTypeAndCmd{
				Frontend: &Cmd{
					CmdInputs: &CmdInputs{
						NodeType: false,
					},
				},
				Automate: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:      "sudo chef-automate config show",
						Single:   false,
						NodeType: true,
					},
				},
				Chef_server: &Cmd{
					CmdInputs: &CmdInputs{
						NodeType: false,
					}},
				Postgresql: &Cmd{
					CmdInputs: &CmdInputs{
						NodeType: false,
					}},
				Opensearch: &Cmd{
					CmdInputs: &CmdInputs{
						NodeType: false,
					}},
				Infra: infra,
			},
			sshUtil: GetMockSSHUtil(&SSHConfig{}, nil, "config show operation completed", nil, "", nil),
			isError: false,
			err:     nil,
		},
		{
			input: &NodeTypeAndCmd{
				Frontend: &Cmd{
					CmdInputs: &CmdInputs{
						NodeType: false,
					},
				},
				Automate: &Cmd{
					CmdInputs: &CmdInputs{
						NodeType: false,
					},
				},
				Chef_server: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:      "sudo chef-automate config show",
						Single:   false,
						NodeType: true,
					}},
				Postgresql: &Cmd{
					CmdInputs: &CmdInputs{
						NodeType: false,
					}},
				Opensearch: &Cmd{
					CmdInputs: &CmdInputs{
						NodeType: false,
					}},
				Infra: infra,
			},
			sshUtil: GetMockSSHUtil(&SSHConfig{}, nil, "config show operation completed", nil, "", nil),
			isError: false,
			err:     nil,
		},
		{
			input: &NodeTypeAndCmd{
				Frontend: &Cmd{
					CmdInputs: &CmdInputs{
						NodeType: false,
					},
				},
				Automate: &Cmd{
					CmdInputs: &CmdInputs{
						NodeType: false,
					},
				},
				Chef_server: &Cmd{
					CmdInputs: &CmdInputs{
						NodeType: false,
					}},
				Postgresql: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:      fmt.Sprintf(GET_BACKEND_CONFIG, "postgresql", ""),
						Single:   false,
						NodeType: true,
					}},
				Opensearch: &Cmd{
					CmdInputs: &CmdInputs{
						NodeType: false,
					}},
				Infra: infra,
			},
			sshUtil: GetMockSSHUtil(&SSHConfig{}, nil, "config show operation completed", nil, "", nil),
			isError: false,
			err:     nil,
		},
		{
			input: &NodeTypeAndCmd{
				Frontend: &Cmd{
					CmdInputs: &CmdInputs{
						NodeType: false,
					},
				},
				Automate: &Cmd{
					CmdInputs: &CmdInputs{
						NodeType: false,
					},
				},
				Chef_server: &Cmd{
					CmdInputs: &CmdInputs{
						NodeType: false,
					}},
				Postgresql: &Cmd{
					CmdInputs: &CmdInputs{
						NodeType: false,
					}},
				Opensearch: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:      fmt.Sprintf(GET_BACKEND_CONFIG, "opensearch", ""),
						Single:   false,
						NodeType: true,
					}},
				Infra: infra,
			},
			sshUtil: GetMockSSHUtil(&SSHConfig{}, nil, "config show operation completed", nil, "", nil),
			isError: false,
			err:     nil,
		},
	}

	for _, testCase := range testCases {
		new := &CmdUtilImpl{
			NodeMap: testCase.input,
			SshUtil: testCase.sshUtil,
		}

		err := new.RunCommand()
		if testCase.isError {
			assert.Error(t, err)
			assert.EqualError(t, testCase.err, err.Error())
		} else {
			assert.NoError(t, err)
		}
	}
}

func TestRemoteJobs(t *testing.T) {
	timestamp := time.Now().Format("20060102150405")
	file := "new.toml"
	resultChan := make(chan CmdResult, 1)

	testCases := []struct {
		testCaseName   string
		command        string
		inputFiles     map[string]string
		outputFiles    []string
		configFile     string
		remoteService  string
		newSSHUtil     SSHUtil
		resultChan     chan CmdResult
		expectedOutput CmdResult
		isError        bool
	}{
		{
			testCaseName: "Successful Copy file to Remote and Execution of command on remote",
			command:      "sudo chef-automate config patch new.toml",
			inputFiles: map[string]string{
				file: "frontend" + "_" + timestamp + "_" + file,
			},
			outputFiles:   []string{},
			remoteService: "automate",
			newSSHUtil:    GetMockSSHUtil(&SSHConfig{hostIP: "127.0.0.3"}, nil, "config patch operation completed", nil, "", nil),
			resultChan:    resultChan,
			expectedOutput: CmdResult{
				HostIP: "127.0.0.3",
				Output: "config patch operation completed",
				Error:  nil,
			},
			isError: false,
		},
		{
			testCaseName:  "Successful Execution of command on remote and Copy file from remote",
			command:       "sudo chef-automate config show new.toml",
			inputFiles:    map[string]string{},
			outputFiles:   []string{"new.toml"},
			remoteService: "automate",
			newSSHUtil:    GetMockSSHUtil(&SSHConfig{hostIP: "127.0.0.3"}, nil, "config show operation completed", nil, "new.toml", nil),
			resultChan:    resultChan,
			expectedOutput: CmdResult{
				HostIP:      "127.0.0.3",
				OutputFiles: []string{"new.toml"},
				Output:      "config show operation completed",
				Error:       nil,
			},
			isError: false,
		},
		{
			testCaseName: "Failed Copy file to Remote",
			command:      "sudo chef-automate config patch new.toml",
			inputFiles: map[string]string{
				file: "frontend" + "_" + timestamp + "_" + file,
			},
			outputFiles:   []string{},
			remoteService: "automate",
			newSSHUtil:    GetMockSSHUtil(&SSHConfig{hostIP: "127.0.0.3"}, errors.Errorf("remote copy"), "", nil, "new.toml", nil),
			resultChan:    resultChan,
			expectedOutput: CmdResult{
				HostIP:      "127.0.0.3",
				OutputFiles: []string{},
				Output:      "",
				Error:       errors.Errorf("remote copy"),
			},
			isError: true,
		},
		{
			testCaseName:  "Failed Execution of command on remote",
			command:       "sudo chef-automate config show",
			inputFiles:    map[string]string{},
			outputFiles:   []string{},
			remoteService: "automate",
			newSSHUtil:    GetMockSSHUtil(&SSHConfig{hostIP: "127.0.0.3"}, nil, "", errors.Errorf("remote execution"), "", nil),
			resultChan:    resultChan,
			expectedOutput: CmdResult{
				HostIP:      "127.0.0.3",
				OutputFiles: []string{},
				Output:      "",
				Error:       errors.Errorf("remote execution"),
			},
			isError: true,
		},
		{
			testCaseName:  "Failed Copy file from remote",
			command:       "sudo chef-automate config show new.toml",
			inputFiles:    map[string]string{},
			outputFiles:   []string{"new.toml"},
			remoteService: "automate",
			newSSHUtil:    GetMockSSHUtil(&SSHConfig{hostIP: "127.0.0.3"}, nil, "", nil, "new.toml", errors.Errorf("remote copy")),
			resultChan:    resultChan,
			expectedOutput: CmdResult{
				HostIP:      "127.0.0.3",
				OutputFiles: []string{"new.toml"},
				Output:      "",
				Error:       errors.Errorf("remote copy"),
			},
			isError: true,
		},
		{
			testCaseName: "Successful Execution of command on remote with error output",
			command:      "sudo chef-automate config patch new.toml",
			inputFiles: map[string]string{
				file: "frontend" + "_" + timestamp + "_" + file,
			},
			outputFiles:   []string{},
			remoteService: "automate",
			newSSHUtil:    GetMockSSHUtil(&SSHConfig{hostIP: "127.0.0.3"}, nil, "Error: patch failed", nil, "new.toml", nil),
			resultChan:    resultChan,
			expectedOutput: CmdResult{
				HostIP:      "127.0.0.3",
				OutputFiles: []string{},
				Output:      "",
				Error:       errors.Errorf("Error: patch failed"),
			},
			isError: true,
		},
	}

	for _, testCase := range testCases {
		new := &CmdUtilImpl{}

		new.RemoteJobs(testCase.command, testCase.inputFiles, testCase.outputFiles, testCase.remoteService, true, testCase.newSSHUtil, testCase.resultChan)
		result := <-resultChan

		if testCase.isError {
			assert.Error(t, result.Error)
			assert.EqualError(t, testCase.expectedOutput.Error, result.Error.Error())
			assert.Equal(t, testCase.expectedOutput.Output, result.Output)
		} else {
			assert.NoError(t, result.Error)
			assert.Equal(t, testCase.expectedOutput.Output, result.Output)
		}
	}
}

func TestIsFrontendIPs(t *testing.T) {
	infra := &AutomteHAInfraDetails{}
	infra.Outputs.AutomatePrivateIps.Value = []string{"127.0.0.3", "127.0.0.4", "127.0.0.5"}
	infra.Outputs.ChefServerPrivateIps.Value = []string{"127.0.0.6", "127.0.0.7", "127.0.0.8"}
	testCases := []struct {
		ip             string
		infra          *AutomteHAInfraDetails
		expectedOutput []string
		isError        bool
		err            error
	}{
		{
			ip:             "127.0.0.4",
			infra:          infra,
			expectedOutput: []string{"127.0.0.4"},
			isError:        false,
			err:            nil,
		},
		{
			ip:             "",
			infra:          infra,
			expectedOutput: []string{"127.0.0.3", "127.0.0.4", "127.0.0.5", "127.0.0.6", "127.0.0.7", "127.0.0.8"},
			isError:        false,
			err:            nil,
		},
		{
			ip:             "127.0.0.1",
			infra:          infra,
			expectedOutput: []string{},
			isError:        true,
			err:            errors.New("Please Enter Valid frontend IP"),
		},
	}

	for _, testCase := range testCases {
		new := &CmdUtilImpl{}

		nodeIps, err := new.IsFrontendIPs(testCase.ip, testCase.infra)
		if testCase.isError {
			assert.Error(t, err)
			assert.EqualError(t, testCase.err, err.Error())
		} else {
			assert.NoError(t, err)
			assert.Equal(t, testCase.expectedOutput, nodeIps)
		}
	}
}

func TestIsAutomateIPs(t *testing.T) {
	infra := &AutomteHAInfraDetails{}
	infra.Outputs.AutomatePrivateIps.Value = []string{"127.0.0.3", "127.0.0.4", "127.0.0.5"}
	testCases := []struct {
		single         bool
		ip             string
		infra          *AutomteHAInfraDetails
		expectedOutput []string
		isError        bool
		err            error
	}{
		{
			single:         true,
			ip:             "",
			infra:          infra,
			expectedOutput: []string{"127.0.0.3"},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             "127.0.0.4",
			infra:          infra,
			expectedOutput: []string{"127.0.0.4"},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             "",
			infra:          infra,
			expectedOutput: []string{"127.0.0.3", "127.0.0.4", "127.0.0.5"},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             "127.0.0.1",
			infra:          infra,
			expectedOutput: []string{},
			isError:        true,
			err:            errors.New("Please Enter Valid automate IP"),
		},
	}

	for _, testCase := range testCases {
		new := &CmdUtilImpl{}

		nodeIps, err := new.IsAutomateIPs(testCase.single, testCase.ip, testCase.infra)
		if testCase.isError {
			assert.Error(t, err)
			assert.EqualError(t, testCase.err, err.Error())
		} else {
			assert.NoError(t, err)
			assert.Equal(t, testCase.expectedOutput, nodeIps)
		}
	}
}

func TestIsChefserverIPs(t *testing.T) {
	infra := &AutomteHAInfraDetails{}
	infra.Outputs.ChefServerPrivateIps.Value = []string{"127.0.0.3", "127.0.0.4", "127.0.0.5"}
	testCases := []struct {
		single         bool
		ip             string
		infra          *AutomteHAInfraDetails
		expectedOutput []string
		isError        bool
		err            error
	}{
		{
			single:         true,
			ip:             "",
			infra:          infra,
			expectedOutput: []string{"127.0.0.3"},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             "127.0.0.4",
			infra:          infra,
			expectedOutput: []string{"127.0.0.4"},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             "",
			infra:          infra,
			expectedOutput: []string{"127.0.0.3", "127.0.0.4", "127.0.0.5"},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             "127.0.0.1",
			infra:          infra,
			expectedOutput: []string{},
			isError:        true,
			err:            errors.New("Please Enter Valid chef-server IP"),
		},
	}

	for _, testCase := range testCases {
		new := &CmdUtilImpl{}

		nodeIps, err := new.IsChefserverIPs(testCase.single, testCase.ip, testCase.infra)
		if testCase.isError {
			assert.Error(t, err)
			assert.EqualError(t, testCase.err, err.Error())
		} else {
			assert.NoError(t, err)
			assert.Equal(t, testCase.expectedOutput, nodeIps)
		}
	}
}

func TestIsPostgresqlIPs(t *testing.T) {
	infra := &AutomteHAInfraDetails{}
	infra.Outputs.PostgresqlPrivateIps.Value = []string{"127.0.0.3", "127.0.0.4", "127.0.0.5"}
	testCases := []struct {
		single         bool
		ip             string
		infra          *AutomteHAInfraDetails
		expectedOutput []string
		isError        bool
		err            error
	}{
		{
			single:         true,
			ip:             "",
			infra:          infra,
			expectedOutput: []string{"127.0.0.3"},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             "127.0.0.4",
			infra:          infra,
			expectedOutput: []string{"127.0.0.4"},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             "",
			infra:          infra,
			expectedOutput: []string{"127.0.0.3", "127.0.0.4", "127.0.0.5"},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             "127.0.0.1",
			infra:          infra,
			expectedOutput: []string{},
			isError:        true,
			err:            errors.New("Please Enter Valid postgresql IP"),
		},
	}

	for _, testCase := range testCases {
		new := &CmdUtilImpl{}

		nodeIps, err := new.IsPostgresqlIPs(testCase.single, testCase.ip, testCase.infra)
		if testCase.isError {
			assert.Error(t, err)
			assert.EqualError(t, testCase.err, err.Error())
		} else {
			assert.NoError(t, err)
			assert.Equal(t, testCase.expectedOutput, nodeIps)
		}
	}
}

func TestIsOpensearchIPs(t *testing.T) {
	infra := &AutomteHAInfraDetails{}
	infra.Outputs.OpensearchPrivateIps.Value = []string{"127.0.0.3", "127.0.0.4", "127.0.0.5"}
	testCases := []struct {
		single         bool
		ip             string
		infra          *AutomteHAInfraDetails
		expectedOutput []string
		isError        bool
		err            error
	}{
		{
			single:         true,
			ip:             "",
			infra:          infra,
			expectedOutput: []string{"127.0.0.3"},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             "127.0.0.4",
			infra:          infra,
			expectedOutput: []string{"127.0.0.4"},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             "",
			infra:          infra,
			expectedOutput: []string{"127.0.0.3", "127.0.0.4", "127.0.0.5"},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             "127.0.0.1",
			infra:          infra,
			expectedOutput: []string{},
			isError:        true,
			err:            errors.New("Please Enter Valid opensearch IP"),
		},
	}

	for _, testCase := range testCases {
		new := &CmdUtilImpl{}

		nodeIps, err := new.IsOpensearchIPs(testCase.single, testCase.ip, testCase.infra)
		if testCase.isError {
			assert.Error(t, err)
			assert.EqualError(t, testCase.err, err.Error())
		} else {
			assert.NoError(t, err)
			assert.Equal(t, testCase.expectedOutput, nodeIps)
		}
	}
}

func GetMockSSHUtil(sshConfig *SSHConfig, CopyToRemoteError error, CmdExecOutput string, CmdExecError error, outputFileName string, CopyFromRemoteError error) *MockSSHUtilsImpl {
	return &MockSSHUtilsImpl{
		getSSHConfigFunc: func() *SSHConfig {
			return sshConfig
		},
		setSSHConfigFunc: func(sshConfig *SSHConfig) {
		},
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return CmdExecOutput, CmdExecError
		},
		copyFileToRemoteFunc: func(srcFilePath string, destFileName string, removeFile bool) error {
			return CopyToRemoteError
		},
		copyFileFromRemoteFunc: func(remoteFilePath string, outputFileName string) (string, error) {
			return outputFileName, CopyFromRemoteError
		},
	}
}
