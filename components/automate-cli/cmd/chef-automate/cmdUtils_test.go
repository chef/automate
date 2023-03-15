package main

import (
	"fmt"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
)

const (
	ip4              = "127.0.0.4"
	ip5              = "127.0.0.5"
	ip6              = "127.0.0.6"
	ip7              = "127.0.0.7"
	ip8              = "127.0.0.8"
	patchCommand     = "sudo chef-automate config patch new.toml"
	showCommand      = "sudo chef-automate config show"
	file             = "new.toml"
	completedMessage = "config show operation completed"
)

func newTmpDir(t testing.TB) string {
	tmpDir := t.TempDir()
	os.MkdirAll(tmpDir, os.ModePerm)
	return tmpDir
}

func TestExecute(t *testing.T) {
	infra := &AutomteHAInfraDetails{}
	infra.Outputs.SSHUser.Value = "ubuntu"
	infra.Outputs.SSHKeyFile.Value = "new.pem"
	infra.Outputs.SSHPort.Value = "22"
	infra.Outputs.AutomatePrivateIps.Value = []string{ip1, ip2, ip3}
	infra.Outputs.ChefServerPrivateIps.Value = []string{ip1, ip2, ip3}
	infra.Outputs.PostgresqlPrivateIps.Value = []string{ip1, ip2, ip3}
	infra.Outputs.OpensearchPrivateIps.Value = []string{ip1, ip2, ip3}
	type fields struct {
		NodeMap *NodeTypeAndCmd
		SshUtil SSHUtil
	}
	testCases := []struct {
		name        string
		fields      fields
		expectedErr error
		wantErr     bool
	}{
		{
			name: "Successful command execution on frontend",
			fields: fields{
				NodeMap: &NodeTypeAndCmd{
					Frontend: &Cmd{
						PreExec: func(cmdInputs *CmdInputs, sshUtil SSHUtil, infra *AutomteHAInfraDetails, remoteService string, timestamp string, writer *cli.Writer) error {
							return nil
						},
						CmdInputs: &CmdInputs{
							Cmd:      showCommand,
							Single:   false,
							NodeType: true,
						},
					},
					Infra: infra,
				},
				SshUtil: GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			},
			expectedErr: nil,
			wantErr:     false,
		},
		{
			name: "Successful command execution on automate",
			fields: fields{
				NodeMap: &NodeTypeAndCmd{
					Frontend: &Cmd{
						CmdInputs: &CmdInputs{
							NodeType: false,
						},
					},
					Automate: &Cmd{
						CmdInputs: &CmdInputs{
							Cmd:        patchCommand,
							InputFiles: []string{file},
							Single:     false,
							NodeType:   true,
						},
					},
					Infra: infra,
				},
				SshUtil: GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			},
			expectedErr: nil,
			wantErr:     false,
		},
		{
			name: "Successful command execution on chef_server",
			fields: fields{
				NodeMap: &NodeTypeAndCmd{
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
					ChefServer: &Cmd{
						CmdInputs: &CmdInputs{
							Cmd:      showCommand,
							Single:   false,
							NodeType: true,
						},
					},
					Infra: infra,
				},
				SshUtil: GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			},
			expectedErr: nil,
			wantErr:     false,
		},
		{
			name: "Successful command execution on postgres",
			fields: fields{
				NodeMap: &NodeTypeAndCmd{
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
					ChefServer: &Cmd{
						CmdInputs: &CmdInputs{
							NodeType: false,
						}},
					Postgresql: &Cmd{
						CmdInputs: &CmdInputs{
							Cmd:      fmt.Sprintf(GET_BACKEND_CONFIG, "postgresql", ""),
							Single:   false,
							NodeType: true,
						}},
					Infra: infra,
				},
				SshUtil: GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			},
			expectedErr: nil,
			wantErr:     false,
		},
		{
			name: "Successful command execution on opensearch",
			fields: fields{
				NodeMap: &NodeTypeAndCmd{
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
					ChefServer: &Cmd{
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
				SshUtil: GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			},
			expectedErr: nil,
			wantErr:     false,
		},
		{
			name: "Node type not supported",
			fields: fields{
				NodeMap: &NodeTypeAndCmd{
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
					ChefServer: &Cmd{
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
				SshUtil: GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			},
			expectedErr: errors.New("Missing or Unsupported flag"),
			wantErr:     true,
		},
	}
	for _, testCase := range testCases {
		t.Run(testCase.name, func(t *testing.T) {
			c := &remoteCmdExecutor{
				NodeMap: testCase.fields.NodeMap,
				SshUtil: testCase.fields.SshUtil,
			}
			err := c.Execute()
			if testCase.wantErr {
				assert.Error(t, err)
				assert.EqualError(t, testCase.expectedErr, err.Error())
			} else {
				assert.NoError(t, err)
			}
		})
	}
}
func TestExecuteCmdOnNode(t *testing.T) {
	timestamp := time.Now().Format("20060102150405")
	file := file
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
			command:      patchCommand,
			inputFiles: map[string]string{
				file: "frontend" + "_" + timestamp + "_" + file,
			},
			outputFiles:   []string{},
			remoteService: "automate",
			newSSHUtil:    GetMockSSHUtil(&SSHConfig{hostIP: ip1}, nil, "config patch operation completed", nil, "", nil),
			resultChan:    resultChan,
			expectedOutput: CmdResult{
				HostIP: ip1,
				Output: "config patch operation completed",
				Error:  nil,
			},
			isError: false,
		},
		{
			testCaseName:  "Successful Execution of command on remote and Copy file from remote",
			command:       "sudo chef-automate config show new.toml",
			inputFiles:    map[string]string{},
			outputFiles:   []string{file},
			remoteService: "automate",
			newSSHUtil:    GetMockSSHUtil(&SSHConfig{hostIP: ip1}, nil, completedMessage, nil, file, nil),
			resultChan:    resultChan,
			expectedOutput: CmdResult{
				HostIP:      ip1,
				OutputFiles: []string{file},
				Output:      completedMessage,
				Error:       nil,
			},
			isError: false,
		},
		{
			testCaseName: "Failed Copy file to Remote",
			command:      patchCommand,
			inputFiles: map[string]string{
				file: "frontend" + "_" + timestamp + "_" + file,
			},
			outputFiles:   []string{},
			remoteService: "automate",
			newSSHUtil:    GetMockSSHUtil(&SSHConfig{hostIP: ip1}, errors.Errorf("copy to remote failed"), "", nil, file, nil),
			resultChan:    resultChan,
			expectedOutput: CmdResult{
				HostIP:      ip1,
				OutputFiles: []string{},
				Output:      "",
				Error:       errors.Errorf("copy to remote failed"),
			},
			isError: true,
		},
		{
			testCaseName:  "Failed Execution of command on remote",
			command:       showCommand,
			inputFiles:    map[string]string{},
			outputFiles:   []string{},
			remoteService: "automate",
			newSSHUtil:    GetMockSSHUtil(&SSHConfig{hostIP: ip1}, nil, "", errors.Errorf("remote execution"), "", nil),
			resultChan:    resultChan,
			expectedOutput: CmdResult{
				HostIP:      ip1,
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
			outputFiles:   []string{file},
			remoteService: "automate",
			newSSHUtil:    GetMockSSHUtil(&SSHConfig{hostIP: ip1}, nil, "", nil, file, errors.Errorf("copy from remote failed")),
			resultChan:    resultChan,
			expectedOutput: CmdResult{
				HostIP:      ip1,
				OutputFiles: []string{file},
				Output:      "",
				Error:       errors.Errorf("copy from remote failed"),
			},
			isError: true,
		},
		{
			testCaseName: "Successful Execution of command on remote with error output",
			command:      patchCommand,
			inputFiles: map[string]string{
				file: "frontend" + "_" + timestamp + "_" + file,
			},
			outputFiles:   []string{},
			remoteService: "automate",
			newSSHUtil:    GetMockSSHUtil(&SSHConfig{hostIP: ip1}, nil, "Error: patch failed", nil, file, nil),
			resultChan:    resultChan,
			expectedOutput: CmdResult{
				HostIP:      ip1,
				OutputFiles: []string{},
				Output:      "",
				Error:       errors.Errorf("Error: patch failed"),
			},
			isError: true,
		},
	}

	for _, testCase := range testCases {
		new := &remoteCmdExecutor{}

		new.executeCmdOnNode(testCase.command, testCase.inputFiles, testCase.outputFiles, testCase.remoteService, true, testCase.newSSHUtil, testCase.resultChan)
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

func TestGetFrontendIPs(t *testing.T) {
	infra := &AutomteHAInfraDetails{}
	infra.Outputs.AutomatePrivateIps.Value = []string{ip1, ip2, ip3}
	infra.Outputs.ChefServerPrivateIps.Value = []string{ip6, ip7, ip8}
	testCases := []struct {
		ip             string
		infra          *AutomteHAInfraDetails
		expectedOutput []string
		isError        bool
		err            error
	}{
		{
			ip:             ip2,
			infra:          infra,
			expectedOutput: []string{ip2},
			isError:        false,
			err:            nil,
		},
		{
			ip:             "",
			infra:          infra,
			expectedOutput: []string{ip1, ip2, ip3, ip6, ip7, ip8},
			isError:        false,
			err:            nil,
		},
		{
			ip:             ip4,
			infra:          infra,
			expectedOutput: []string{},
			isError:        true,
			err:            errors.New("Please Enter Valid frontend IP"),
		},
	}

	for _, testCase := range testCases {
		nodeIps, err := getFrontendIPs(testCase.ip, testCase.infra)
		if testCase.isError {
			assert.Error(t, err)
			assert.EqualError(t, testCase.err, err.Error())
		} else {
			assert.NoError(t, err)
			assert.Equal(t, testCase.expectedOutput, nodeIps)
		}
	}
}

func TestGetAutomateIPs(t *testing.T) {
	infra := &AutomteHAInfraDetails{}
	infra.Outputs.AutomatePrivateIps.Value = []string{ip1, ip2, ip3}
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
			expectedOutput: []string{ip1},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             ip2,
			infra:          infra,
			expectedOutput: []string{ip2},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             "",
			infra:          infra,
			expectedOutput: []string{ip1, ip2, ip3},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             ip5,
			infra:          infra,
			expectedOutput: []string{},
			isError:        true,
			err:            errors.New("Please Enter Valid automate IP"),
		},
	}

	for _, testCase := range testCases {
		nodeIps, err := getAutomateIPs(testCase.single, testCase.ip, testCase.infra)
		if testCase.isError {
			assert.Error(t, err)
			assert.EqualError(t, testCase.err, err.Error())
		} else {
			assert.NoError(t, err)
			assert.Equal(t, testCase.expectedOutput, nodeIps)
		}
	}
}

func TestGetChefserverIPs(t *testing.T) {
	infra := &AutomteHAInfraDetails{}
	infra.Outputs.ChefServerPrivateIps.Value = []string{ip1, ip2, ip3}
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
			expectedOutput: []string{ip1},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             ip2,
			infra:          infra,
			expectedOutput: []string{ip2},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             "",
			infra:          infra,
			expectedOutput: []string{ip1, ip2, ip3},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             ip5,
			infra:          infra,
			expectedOutput: []string{},
			isError:        true,
			err:            errors.New("Please Enter Valid chef-server IP"),
		},
	}

	for _, testCase := range testCases {
		nodeIps, err := getChefserverIPs(testCase.single, testCase.ip, testCase.infra)
		if testCase.isError {
			assert.Error(t, err)
			assert.EqualError(t, testCase.err, err.Error())
		} else {
			assert.NoError(t, err)
			assert.Equal(t, testCase.expectedOutput, nodeIps)
		}
	}
}

func TestGetPostgresqlIPs(t *testing.T) {
	infra := &AutomteHAInfraDetails{}
	infra.Outputs.PostgresqlPrivateIps.Value = []string{ip1, ip2, ip3}
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
			expectedOutput: []string{ip1},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             ip2,
			infra:          infra,
			expectedOutput: []string{ip2},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             "",
			infra:          infra,
			expectedOutput: []string{ip1, ip2, ip3},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             ip5,
			infra:          infra,
			expectedOutput: []string{},
			isError:        true,
			err:            errors.New("Please Enter Valid postgresql IP"),
		},
	}

	for _, testCase := range testCases {
		nodeIps, err := getPostgresqlIPs(testCase.single, testCase.ip, testCase.infra)
		if testCase.isError {
			assert.Error(t, err)
			assert.EqualError(t, testCase.err, err.Error())
		} else {
			assert.NoError(t, err)
			assert.Equal(t, testCase.expectedOutput, nodeIps)
		}
	}
}

func TestGetOpensearchIPs(t *testing.T) {
	infra := &AutomteHAInfraDetails{}
	infra.Outputs.OpensearchPrivateIps.Value = []string{ip1, ip2, ip3}
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
			expectedOutput: []string{ip1},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             ip2,
			infra:          infra,
			expectedOutput: []string{ip2},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             "",
			infra:          infra,
			expectedOutput: []string{ip1, ip2, ip3},
			isError:        false,
			err:            nil,
		},
		{
			single:         false,
			ip:             ip5,
			infra:          infra,
			expectedOutput: []string{},
			isError:        true,
			err:            errors.New("Please Enter Valid opensearch IP"),
		},
	}

	for _, testCase := range testCases {
		nodeIps, err := getOpensearchIPs(testCase.single, testCase.ip, testCase.infra)
		if testCase.isError {
			assert.Error(t, err)
			assert.EqualError(t, testCase.err, err.Error())
		} else {
			assert.NoError(t, err)
			assert.Equal(t, testCase.expectedOutput, nodeIps)
		}
	}
}

func TestPrintOutput(t *testing.T) {
	tmpDir := newTmpDir(t)
	out := filepath.Join(tmpDir, file)
	fail := filepath.Join("2342"+tmpDir, file)
	result := filepath.Join(tmpDir, file)
	type args struct {
		remoteService string
		result        CmdResult
		outputFiles   []string
		cliWriter     *cli.Writer
	}
	tests := []struct {
		name string
		args args
	}{
		{
			name: "Print Success message and combine output files in one",
			args: args{
				remoteService: CONST_AUTOMATE,
				result: CmdResult{
					HostIP:      ip7,
					OutputFiles: []string{out},
					Output:      completedMessage,
					Error:       nil,
				},
				outputFiles: []string{result},
				cliWriter:   getMockWriterImpl(),
			},
		},
		{
			name: "Print Success message and file combinations failed",
			args: args{
				remoteService: CONST_AUTOMATE,
				result: CmdResult{
					HostIP:      ip7,
					OutputFiles: []string{fail},
					Output:      completedMessage,
					Error:       nil,
				},
				outputFiles: []string{result},
				cliWriter:   getMockWriterImpl(),
			},
		},
		{
			name: "Print Error message",
			args: args{
				remoteService: CONST_AUTOMATE,
				result: CmdResult{
					HostIP:      ip7,
					OutputFiles: []string{},
					Output:      "",
					Error:       errors.New("Patch failed"),
				},
				outputFiles: []string{},
				cliWriter:   getMockWriterImpl(),
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			printOutput(tt.args.remoteService, tt.args.result, tt.args.outputFiles, tt.args.cliWriter)
		})
	}
}

func GetMockSSHUtil(sshConfig *SSHConfig, CopyToRemoteError error, CmdExecOutput string, CmdExecError error, outputFileName string, CopyFromRemoteError error) *MockSSHUtilsImpl {
	return &MockSSHUtilsImpl{
		getSSHConfigFunc: func() *SSHConfig {
			return sshConfig
		},
		setSSHConfigFunc: func(sshConfig *SSHConfig) {
			// No return for this function
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
