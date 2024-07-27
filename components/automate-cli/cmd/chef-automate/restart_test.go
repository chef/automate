package main

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/logger"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
)

func TestConstructNodeMapForAllNodeTypes(t *testing.T) {
	type testCase struct {
		flags           *RestartCmdFlags
		nodeMapExpected *NodeTypeAndCmd
	}
	infra := &AutomateHAInfraDetails{}

	testCases := []testCase{
		{
			flags: &RestartCmdFlags{
				automate: true,
			},
			nodeMapExpected: &NodeTypeAndCmd{
				Frontend: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:                      RESTART_FRONTEND_COMMAND,
						HideSSHConnectionMessage: true,
					},
				},
				Automate: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:                      RESTART_FRONTEND_COMMAND,
						ErrorCheckEnableInOutput: true,
						NodeIps:                  []string{""},
						NodeType:                 true,
						SkipPrintOutput:          true,
						HideSSHConnectionMessage: true,
					},
				},
				ChefServer: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:                      RESTART_FRONTEND_COMMAND,
						ErrorCheckEnableInOutput: true,
						NodeIps:                  []string{""},
						NodeType:                 false,
						SkipPrintOutput:          true,
						HideSSHConnectionMessage: true,
					},
				},
				Postgresql: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:                      RESTART_BACKEND_COMMAND,
						NodeIps:                  []string{""},
						ErrorCheckEnableInOutput: true,
						NodeType:                 false,
						SkipPrintOutput:          true,
						HideSSHConnectionMessage: true,
					},
				},
				Opensearch: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:                      RESTART_BACKEND_COMMAND,
						NodeIps:                  []string{""},
						ErrorCheckEnableInOutput: true,
						NodeType:                 false,
						SkipPrintOutput:          true,
						HideSSHConnectionMessage: true,
					},
				},
				Infra: infra,
			},
		},
	}

	for _, testCase := range testCases {
		nodeMapGet := constructNodeMapForAllNodeTypes(testCase.flags, infra)
		assert.Equal(t, testCase.nodeMapExpected, nodeMapGet)
	}
}

func newDefaultRestartCmdFlag() *RestartCmdFlags {
	return &RestartCmdFlags{
		automate:   false,
		chefServer: false,
		postgresql: false,
		opensearch: false,
		node:       "",
		timeout:    DEFAULT_TIMEOUT_FOR_RESTART,
	}
}
func TestRunRestartFromBastion(t *testing.T) {
	type testCase struct {
		description          string
		flags                *RestartCmdFlags
		mockRestartCmdHelper *MockNodeUtilsImpl
		mockRemoteCmdExec    *MockRemoteCmdExecutor
		errorWant            error
	}
	printRestartOutput := func(m map[string][]*CmdResult, s string, w *cli.Writer) {}
	mockNodeUtils := &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			return getMockInfra(), &SSHConfig{}, nil
		},
		isManagedServicesOnFunc: func() bool {
			return false
		},
		postPGCertRotateFunc: func(pgIps []string, sshconfig SSHConfig, fileUtils fileutils.FileUtils, log logger.Logger) error {
			return nil
		},
		getConfigPullerFunc: func(sshUtil *SSHUtil) (PullConfigs, error) {
			return &MockPullConfigs{}, nil
		},
		restartPgNodesFunc: func(leaderNode NodeIpHealth, pgIps []string, infra *AutomateHAInfraDetails, statusSummary StatusSummary) error {
			return nil
		},
	}
	testCases := []testCase{
		{
			description: "Error when wait timeout is less than default timeout",
			flags: func() *RestartCmdFlags {
				restartCmdFlags := newDefaultRestartCmdFlag()
				restartCmdFlags.timeout = 120
				return restartCmdFlags
			}(),
			mockRestartCmdHelper: mockNodeUtils,
			mockRemoteCmdExec: &MockRemoteCmdExecutor{
				ExecuteWithNodeMapFunc: func(nodemap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, nil
				},
			},
			errorWant: status.Errorf(status.InvalidCommandArgsError, "The operation timeout duration for each individual node during the restart should be set to a value greater than %v seconds.", DEFAULT_TIMEOUT_FOR_RESTART),
		},
		{
			description: "No service flag provided and node flag is provided",
			flags: func() *RestartCmdFlags {
				restartCmdFlags := newDefaultRestartCmdFlag()
				restartCmdFlags.node = "1"
				return restartCmdFlags
			}(),
			mockRestartCmdHelper: mockNodeUtils,
			mockRemoteCmdExec: &MockRemoteCmdExecutor{
				ExecuteWithNodeMapFunc: func(nodemap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, nil
				},
			},
			errorWant: status.Errorf(status.InvalidCommandArgsError, "Please provide service flag"),
		},
		{
			description: "Error while reading infra details",
			flags: func() *RestartCmdFlags {
				restartCmdFlags := newDefaultRestartCmdFlag()
				return restartCmdFlags
			}(),
			mockRestartCmdHelper: &MockNodeUtilsImpl{
				getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
					return nil, &SSHConfig{}, errors.New("Error occured while reading infra details")
				},
				isManagedServicesOnFunc: func() bool {
					return false
				},
				postPGCertRotateFunc: func(pgIps []string, sshconfig SSHConfig, fileUtils fileutils.FileUtils, log logger.Logger) error {
					return nil
				},
				getConfigPullerFunc: func(sshUtil *SSHUtil) (PullConfigs, error) {
					return &MockPullConfigs{}, nil
				},
				restartPgNodesFunc: func(leaderNode NodeIpHealth, pgIps []string, infra *AutomateHAInfraDetails, statusSummary StatusSummary) error {
					return nil
				},
			},
			mockRemoteCmdExec: &MockRemoteCmdExecutor{
				ExecuteWithNodeMapFunc: func(nodemap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, nil
				},
			},
			errorWant: errors.New("Error occured while reading infra details"),
		},
		{
			description: "Restart all node-types",
			flags: func() *RestartCmdFlags {
				restartCmdFlags := newDefaultRestartCmdFlag()
				return restartCmdFlags
			}(),
			mockRestartCmdHelper: mockNodeUtils,
			mockRemoteCmdExec: &MockRemoteCmdExecutor{
				ExecuteWithNodeMapFunc: func(nodemap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, nil
				},
				SetWriterFunc: func(cli *cli.Writer) {},
			},
			errorWant: nil,
		},
		{
			description: "Error when restarting all node-types with remote execution ",
			flags: func() *RestartCmdFlags {
				restartCmdFlags := newDefaultRestartCmdFlag()
				return restartCmdFlags
			}(),
			mockRestartCmdHelper: mockNodeUtils,
			mockRemoteCmdExec: &MockRemoteCmdExecutor{
				ExecuteWithNodeMapFunc: func(nodemap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, errors.New("Some error occured while remote execution")
				},
				SetWriterFunc: func(cli *cli.Writer) {},
			},
			errorWant: errors.New("Some error occured while remote execution"),
		},
		{
			description: "Restarting all services with managed Infra",
			flags: func() *RestartCmdFlags {
				restartCmdFlags := newDefaultRestartCmdFlag()
				return restartCmdFlags
			}(),
			mockRestartCmdHelper: mockNodeUtils,
			mockRemoteCmdExec: &MockRemoteCmdExecutor{
				ExecuteWithNodeMapFunc: func(nodemap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, nil
				},
				SetWriterFunc: func(cli *cli.Writer) {},
			},
			errorWant: nil,
		},
		{
			description: "Restarting Opensearch with managed services",
			flags: func() *RestartCmdFlags {
				restartCmdFlags := newDefaultRestartCmdFlag()
				restartCmdFlags.opensearch = true
				return restartCmdFlags
			}(),
			mockRestartCmdHelper: &MockNodeUtilsImpl{
				getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
					return getMockInfra(), &SSHConfig{}, nil
				},
				isManagedServicesOnFunc: func() bool {
					return true
				},
				postPGCertRotateFunc: func(pgIps []string, sshconfig SSHConfig, fileUtils fileutils.FileUtils, log logger.Logger) error {
					return nil
				},
				getConfigPullerFunc: func(sshUtil *SSHUtil) (PullConfigs, error) {
					return &MockPullConfigs{}, nil
				},
				restartPgNodesFunc: func(leaderNode NodeIpHealth, pgIps []string, infra *AutomateHAInfraDetails, statusSummary StatusSummary) error {
					return nil
				},
			},
			mockRemoteCmdExec: &MockRemoteCmdExecutor{
				ExecuteWithNodeMapFunc: func(nodemap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, nil
				},
				SetWriterFunc: func(cli *cli.Writer) {},
			},
			errorWant: status.Errorf(status.InvalidCommandArgsError, ERROR_ON_MANAGED_SERVICES, OPENSEARCH),
		},
		{
			description: "Restarting Postgresql with managed services",
			flags: func() *RestartCmdFlags {
				restartCmdFlags := newDefaultRestartCmdFlag()
				restartCmdFlags.postgresql = true
				return restartCmdFlags
			}(),
			mockRestartCmdHelper: &MockNodeUtilsImpl{
				getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
					return getMockInfra(), &SSHConfig{}, nil
				},
				isManagedServicesOnFunc: func() bool {
					return true
				},
				postPGCertRotateFunc: func(pgIps []string, sshconfig SSHConfig, fileUtils fileutils.FileUtils, log logger.Logger) error {
					return nil
				},
				getConfigPullerFunc: func(sshUtil *SSHUtil) (PullConfigs, error) {
					return &MockPullConfigs{}, nil
				},
				restartPgNodesFunc: func(leaderNode NodeIpHealth, pgIps []string, infra *AutomateHAInfraDetails, statusSummary StatusSummary) error {
					return nil
				},
			},
			mockRemoteCmdExec: &MockRemoteCmdExecutor{
				ExecuteWithNodeMapFunc: func(nodemap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, nil
				},
				SetWriterFunc: func(cli *cli.Writer) {},
			},
			errorWant: status.Errorf(status.InvalidCommandArgsError, ERROR_ON_MANAGED_SERVICES, POSTGRESQL),
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.description, func(t *testing.T) {
			getAutomateHAInfraDetailsFunc = func() (*AutomateHAInfraDetails, error) {
				infra := &AutomateHAInfraDetails{}
				infra.Outputs.PostgresqlPrivateIps.Value = []string{ValidIP7, ValidIP8, ValidIP9}
				return infra, nil
			}
			getPGLeaderFunc = func(StatusSummary) *NodeIpHealth {
				return &NodeIpHealth{
					IP:     ValidIP7,
					Health: "OK",
				}
			}
			err := runRestartFromBastion(testCase.flags, testCase.mockRemoteCmdExec, testCase.mockRestartCmdHelper, printRestartOutput, getMockStatusSummary())
			if testCase.errorWant != nil {
				assert.EqualError(t, err, testCase.errorWant.Error())
			} else {
				assert.Nil(t, err)
			}
		})
	}
}

func TestHandleManagedServiceError(t *testing.T) {

	testCases := []struct {
		flags          *RestartCmdFlags
		errorExepected error
	}{
		{
			flags: &RestartCmdFlags{
				postgresql: true,
			},
			errorExepected: status.Errorf(status.InvalidCommandArgsError, ERROR_ON_MANAGED_SERVICES, POSTGRESQL),
		},
		{
			flags: &RestartCmdFlags{
				opensearch: true,
			},
			errorExepected: status.Errorf(status.InvalidCommandArgsError, ERROR_ON_MANAGED_SERVICES, OPENSEARCH),
		},
		{
			flags:          &RestartCmdFlags{},
			errorExepected: nil,
		},
		{
			flags: &RestartCmdFlags{
				postgresql: true,
				opensearch: true,
			},
			errorExepected: status.Errorf(status.InvalidCommandArgsError, ERROR_ON_MANAGED_SERVICES, "postgresql and opensearch"),
		},
	}

	for _, testCase := range testCases {
		err := handleManagedServices(testCase.flags)

		if testCase.errorExepected != nil {
			assert.EqualError(t, err, testCase.errorExepected.Error())
		} else {
			assert.Nil(t, err)
		}
	}
}
