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
	errorForPreExec  = "PreExec function failed"
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

	nodeFlagFalse := &Cmd{
		CmdInputs: &CmdInputs{
			NodeType: false,
			SkipPrintOutput: true,
		},
	}
	preExecFailed := &Cmd{
		PreExec: func(cmdInputs *CmdInputs, sshUtil SSHUtil, infra *AutomteHAInfraDetails, remoteService string, timestamp string, writer *cli.Writer) error {
			return errors.New(errorForPreExec)
		},
		CmdInputs: &CmdInputs{
			NodeType: true,
			SkipPrintOutput: true,
		},
	}
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
							Cmd:             showCommand,
							Single:          false,
							NodeType:        true,
							SkipPrintOutput: true,
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
					Frontend: nodeFlagFalse,
					Automate: &Cmd{
						CmdInputs: &CmdInputs{
							Cmd:             patchCommand,
							InputFiles:      []string{file},
							Single:          false,
							NodeType:        true,
							SkipPrintOutput: true,
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
					Frontend: nodeFlagFalse,
					Automate: nodeFlagFalse,
					ChefServer: &Cmd{
						CmdInputs: &CmdInputs{
							Cmd:             showCommand,
							Single:          false,
							NodeType:        true,
							SkipPrintOutput: true,
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
					Frontend:   nodeFlagFalse,
					Automate:   nodeFlagFalse,
					ChefServer: nodeFlagFalse,
					Postgresql: &Cmd{
						CmdInputs: &CmdInputs{
							Cmd:             fmt.Sprintf(GET_BACKEND_CONFIG, "postgresql", ""),
							Single:          false,
							NodeType:        true,
							SkipPrintOutput: true,
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
					Frontend:   nodeFlagFalse,
					Automate:   nodeFlagFalse,
					ChefServer: nodeFlagFalse,
					Postgresql: nodeFlagFalse,
					Opensearch: &Cmd{
						CmdInputs: &CmdInputs{
							Cmd:             fmt.Sprintf(GET_BACKEND_CONFIG, "opensearch", ""),
							Single:          false,
							NodeType:        true,
							SkipPrintOutput: true,
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
					Frontend:   nodeFlagFalse,
					Automate:   nodeFlagFalse,
					ChefServer: nodeFlagFalse,
					Postgresql: nodeFlagFalse,
					Opensearch: nodeFlagFalse,
					Infra:      infra,
				},
				SshUtil: GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			},
			expectedErr: errors.New("Missing or Unsupported flag"),
			wantErr:     true,
		},
		{
			name: "Failed command execution on frontend as preExec Function failed",
			fields: fields{
				NodeMap: &NodeTypeAndCmd{
					Frontend: preExecFailed,
					Infra:    infra,
				},
				SshUtil: GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			},
			expectedErr: errors.New(errorForPreExec),
			wantErr:     true,
		},
		{
			name: "Failed command execution on automate as preExec Function failed",
			fields: fields{
				NodeMap: &NodeTypeAndCmd{
					Frontend: nodeFlagFalse,
					Automate: preExecFailed,
					Infra:    infra,
				},
				SshUtil: GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			},
			expectedErr: errors.New(errorForPreExec),
			wantErr:     true,
		},
		{
			name: "Failed command execution on chef_server as preExec Function failed",
			fields: fields{
				NodeMap: &NodeTypeAndCmd{
					Frontend:   nodeFlagFalse,
					Automate:   nodeFlagFalse,
					ChefServer: preExecFailed,
					Infra:      infra,
				},
				SshUtil: GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			},
			expectedErr: errors.New(errorForPreExec),
			wantErr:     true,
		},
		{
			name: "Failed command execution on postgres as preExec Function failed",
			fields: fields{
				NodeMap: &NodeTypeAndCmd{
					Frontend:   nodeFlagFalse,
					Automate:   nodeFlagFalse,
					ChefServer: nodeFlagFalse,
					Postgresql: preExecFailed,
					Infra:      infra,
				},
				SshUtil: GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			},
			expectedErr: errors.New(errorForPreExec),
			wantErr:     true,
		},
		{
			name: "Failed command execution on opensearch as preExec Function failed",
			fields: fields{
				NodeMap: &NodeTypeAndCmd{
					Frontend:   nodeFlagFalse,
					Automate:   nodeFlagFalse,
					ChefServer: nodeFlagFalse,
					Postgresql: nodeFlagFalse,
					Opensearch: preExecFailed,
					Infra:      infra,
				},
				SshUtil: GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			},
			expectedErr: errors.New(errorForPreExec),
			wantErr:     true,
		},
	}
	for _, testCase := range testCases {
		t.Run(testCase.name, func(t *testing.T) {
			c := &remoteCmdExecutor{
				NodeMap: testCase.fields.NodeMap,
				SshUtil: testCase.fields.SshUtil,
			}
			_, err := c.Execute()
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
				ScriptName:  "",
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
		new.executeCmdOnNode(testCase.command, "", testCase.inputFiles, testCase.outputFiles, testCase.remoteService, true, testCase.newSSHUtil, testCase.resultChan)
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

func TestPreCmdExecCheck(t *testing.T) {
	infra := &AutomteHAInfraDetails{}
	infra.Outputs.OpensearchPrivateIps.Value = []string{ip1, ip2, ip3}
	type args struct {
		node          *Cmd
		sshUtil       SSHUtil
		infra         *AutomteHAInfraDetails
		remoteService string
		timestamp     string
		writer        *cli.Writer
	}
	tests := []struct {
		name        string
		args        args
		want        []string
		expectedErr error
		wantErr     bool
	}{
		{
			name: "Success: single opensearch node ip",
			args: args{
				node: &Cmd{
					PreExec: func(cmdInputs *CmdInputs, sshUtil SSHUtil, infra *AutomteHAInfraDetails, remoteService string, timestamp string, writer *cli.Writer) error {
						return nil
					},
					CmdInputs: &CmdInputs{
						Single:  true,
						NodeIps: []string{""},
					},
				},
				sshUtil:       GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
				infra:         infra,
				remoteService: "opensearch",
				timestamp:     "20060102150405",
				writer:        getMockWriterImpl(),
			},
			want:        []string{ip1},
			expectedErr: nil,
			wantErr:     false,
		},
		{
			name: "Success: given opensearch node ip",
			args: args{
				node: &Cmd{
					PreExec: func(cmdInputs *CmdInputs, sshUtil SSHUtil, infra *AutomteHAInfraDetails, remoteService string, timestamp string, writer *cli.Writer) error {
						return nil
					},
					CmdInputs: &CmdInputs{
						Single:  false,
						NodeIps: []string{ip2},
					},
				},
				sshUtil:       GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
				infra:         infra,
				remoteService: "opensearch",
				timestamp:     "20060102150405",
				writer:        getMockWriterImpl(),
			},
			want:    []string{ip2},
			wantErr: false,
		},
		{
			name: "Failed: no ips found ",
			args: args{
				node: &Cmd{
					PreExec: func(cmdInputs *CmdInputs, sshUtil SSHUtil, infra *AutomteHAInfraDetails, remoteService string, timestamp string, writer *cli.Writer) error {
						return nil
					},
					CmdInputs: &CmdInputs{},
				},
				sshUtil:       GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
				infra:         infra,
				remoteService: "abcd",
				timestamp:     "20060102150405",
				writer:        getMockWriterImpl(),
			},
			want:        []string{},
			expectedErr: errors.New("No abcd IPs are found"),
			wantErr:     true,
		},
		{
			name: "Failed: given opensearch node ip not found",
			args: args{
				node: &Cmd{
					PreExec: func(cmdInputs *CmdInputs, sshUtil SSHUtil, infra *AutomteHAInfraDetails, remoteService string, timestamp string, writer *cli.Writer) error {
						return nil
					},
					CmdInputs: &CmdInputs{
						Single:  false,
						NodeIps: []string{ip4},
					},
				},
				sshUtil:       GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
				infra:         infra,
				remoteService: "opensearch",
				timestamp:     "20060102150405",
				writer:        getMockWriterImpl(),
			},
			want:        []string{},
			expectedErr: errors.New("Please Enter Valid Node IP"),
			wantErr:     true,
		},
		{
			name: "Failed: given opensearch node ip invalid",
			args: args{
				node: &Cmd{
					PreExec: func(cmdInputs *CmdInputs, sshUtil SSHUtil, infra *AutomteHAInfraDetails, remoteService string, timestamp string, writer *cli.Writer) error {
						return nil
					},
					CmdInputs: &CmdInputs{
						Single:  false,
						NodeIps: []string{"256.255.255.255"},
					},
				},
				sshUtil:       GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
				infra:         infra,
				remoteService: "opensearch",
				timestamp:     "20060102150405",
				writer:        getMockWriterImpl(),
			},
			want:        []string{},
			expectedErr: errors.New("Please Enter Valid Node IP"),
			wantErr:     true,
		},
		{
			name: "Failed: single opensearch node ip",
			args: args{
				node: &Cmd{
					PreExec: func(cmdInputs *CmdInputs, sshUtil SSHUtil, infra *AutomteHAInfraDetails, remoteService string, timestamp string, writer *cli.Writer) error {
						return nil
					},
					CmdInputs: &CmdInputs{
						Single:  true,
						NodeIps: []string{""},
					},
				},
				sshUtil:       GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
				infra:         &AutomteHAInfraDetails{},
				remoteService: "opensearch",
				timestamp:     "20060102150405",
				writer:        getMockWriterImpl(),
			},
			want:        []string{},
			expectedErr: errors.New("No ips found"),
			wantErr:     true,
		},
		{
			name: "Failed: preExec function execution",
			args: args{
				node: &Cmd{
					PreExec: func(cmdInputs *CmdInputs, sshUtil SSHUtil, infra *AutomteHAInfraDetails, remoteService string, timestamp string, writer *cli.Writer) error {
						return errors.New(errorForPreExec)
					},
					CmdInputs: &CmdInputs{
						Single:  false,
						NodeIps: []string{ip2},
					},
				},
				sshUtil:       GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
				infra:         infra,
				remoteService: "opensearch",
				timestamp:     "20060102150405",
				writer:        getMockWriterImpl(),
			},
			want:        []string{},
			expectedErr: errors.New(errorForPreExec),
			wantErr:     true,
		},
	}
	for _, testCase := range tests {
		t.Run(testCase.name, func(t *testing.T) {
			got, err := preCmdExecCheck(testCase.args.node, testCase.args.sshUtil, testCase.args.infra, testCase.args.remoteService, testCase.args.timestamp, testCase.args.writer)
			if testCase.wantErr {
				assert.Error(t, err)
				assert.EqualError(t, testCase.expectedErr, err.Error())
			} else {
				assert.NoError(t, err)
				assert.EqualValues(t, testCase.want, got)
			}
		})
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
