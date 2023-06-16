package main

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
)

type MockrestartFromBastionImpl struct {
	getAutomateHAInfraDetailsFunc func() (*AutomateHAInfraDetails, error)
	isManagedServicesOnFunc       func() bool
	executeRemoteExecutorFunc     func(*NodeTypeAndCmd, SSHUtil, *cli.Writer) (map[string][]*CmdResult, error)
}

func (mrs *MockrestartFromBastionImpl) getAutomateHAInfraDetails() (*AutomateHAInfraDetails, error) {
	return mrs.getAutomateHAInfraDetailsFunc()
}

func (mrs *MockrestartFromBastionImpl) isManagedServicesOn() bool {
	return mrs.isManagedServicesOnFunc()
}

func (mrs *MockrestartFromBastionImpl) executeRemoteExecutor(nodemap *NodeTypeAndCmd, sshUtil SSHUtil, writer *cli.Writer) (map[string][]*CmdResult, error) {
	return mrs.executeRemoteExecutorFunc(nodemap, sshUtil, writer)
}

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
						HideSSHConnectionMessage: true,
					},
				},
				ChefServer: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:                      RESTART_FRONTEND_COMMAND,
						ErrorCheckEnableInOutput: true,
						NodeIps:                  []string{""},
						NodeType:                 false,
						HideSSHConnectionMessage: true,
					},
				},
				Postgresql: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:                      RESTART_BACKEND_COMMAND,
						NodeIps:                  []string{""},
						ErrorCheckEnableInOutput: true,
						NodeType:                 false,
						HideSSHConnectionMessage: true,
					},
				},
				Opensearch: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:                      RESTART_BACKEND_COMMAND,
						NodeIps:                  []string{""},
						ErrorCheckEnableInOutput: true,
						NodeType:                 false,
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
		mockRestartCmdHelper *MockrestartFromBastionImpl
		errorWant            error
	}
	testCases := []testCase{
		{
			description: "Error when wait timeout is less than default timeout",
			flags: func ()*RestartCmdFlags{
				restartCmdFlags := newDefaultRestartCmdFlag()
				restartCmdFlags.timeout=120
				return restartCmdFlags
			  } (),
			mockRestartCmdHelper: &MockrestartFromBastionImpl{
				getAutomateHAInfraDetailsFunc: func() (*AutomateHAInfraDetails, error) {
					return &AutomateHAInfraDetails{}, nil
				},
			},
			errorWant: status.Errorf(status.InvalidCommandArgsError, "The operation timeout duration for each individual node during the restart should be set to a value greater than %v seconds.",DEFAULT_TIMEOUT_FOR_RESTART),
		},
		{
			description: "No service flag provided and node flag is provided",
			flags: func ()*RestartCmdFlags{
				restartCmdFlags := newDefaultRestartCmdFlag()
				restartCmdFlags.node="1"
				return restartCmdFlags
			  } (),
			mockRestartCmdHelper: &MockrestartFromBastionImpl{
				getAutomateHAInfraDetailsFunc: func() (*AutomateHAInfraDetails, error) {
					return &AutomateHAInfraDetails{}, nil
				},
			},
			errorWant: status.Errorf(status.InvalidCommandArgsError, "Please provide service flag"),
		},
		{
			description: "Error while reading infra details",
			flags:      func ()*RestartCmdFlags{
				restartCmdFlags := newDefaultRestartCmdFlag()
				return restartCmdFlags
			  } (),
			mockRestartCmdHelper: &MockrestartFromBastionImpl{
				getAutomateHAInfraDetailsFunc: func() (*AutomateHAInfraDetails, error) {
					return nil, errors.New("Error occured while reading infra details")
				},
			},
			errorWant: errors.New("Error occured while reading infra details"),
		},
		{
			description: "Restart all node-types",
			flags:   func ()*RestartCmdFlags{
				restartCmdFlags := newDefaultRestartCmdFlag()
				return restartCmdFlags
			  } (),
			mockRestartCmdHelper: &MockrestartFromBastionImpl{
				getAutomateHAInfraDetailsFunc: func() (*AutomateHAInfraDetails, error) {
					return &AutomateHAInfraDetails{}, nil
				},
				isManagedServicesOnFunc: func() bool {
					return false
				},
				executeRemoteExecutorFunc: func(ntac *NodeTypeAndCmd, s SSHUtil, w *cli.Writer) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, nil
				},
			},
			errorWant: nil,
		},
		{
			description: "Error when restarting all node-types with remote execution ",
			flags:  func ()*RestartCmdFlags{
				restartCmdFlags := newDefaultRestartCmdFlag()
				return restartCmdFlags
			  } (),
			mockRestartCmdHelper: &MockrestartFromBastionImpl{
				getAutomateHAInfraDetailsFunc: func() (*AutomateHAInfraDetails, error) {
					return &AutomateHAInfraDetails{}, nil
				},
				isManagedServicesOnFunc: func() bool {
					return false
				},
				executeRemoteExecutorFunc: func(ntac *NodeTypeAndCmd, s SSHUtil, w *cli.Writer) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, errors.New("Some error occured while remote execution")
				},
			},
			errorWant: errors.New("Some error occured while remote execution"),
		},
		{
			description: "Restarting all services with managed Infra",
			flags:   func ()*RestartCmdFlags{
				restartCmdFlags := newDefaultRestartCmdFlag()
				return restartCmdFlags
			  } (),
			mockRestartCmdHelper: &MockrestartFromBastionImpl{
				getAutomateHAInfraDetailsFunc: func() (*AutomateHAInfraDetails, error) {
					return &AutomateHAInfraDetails{}, nil
				},
				isManagedServicesOnFunc: func() bool {
					return true
				},
				executeRemoteExecutorFunc: func(ntac *NodeTypeAndCmd, s SSHUtil, w *cli.Writer) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, nil
				},
			},
			errorWant: nil,
		},
		{
			description: "Restarting Opensearch with managed services",
			flags: func ()*RestartCmdFlags{
				restartCmdFlags := newDefaultRestartCmdFlag()
				restartCmdFlags.opensearch = true
				return restartCmdFlags
			  } (),
			mockRestartCmdHelper: &MockrestartFromBastionImpl{
				getAutomateHAInfraDetailsFunc: func() (*AutomateHAInfraDetails, error) {
					return &AutomateHAInfraDetails{}, nil
				},
				isManagedServicesOnFunc: func() bool {
					return true
				},
				executeRemoteExecutorFunc: func(ntac *NodeTypeAndCmd, s SSHUtil, w *cli.Writer) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, nil
				},
			},
			errorWant: status.Errorf(status.InvalidCommandArgsError, ERROR_ON_MANAGED_SERVICES, OPENSEARCH),
		},
		{
			description: "Restarting Postgresql with managed services",
			flags: func ()*RestartCmdFlags{
				restartCmdFlags := newDefaultRestartCmdFlag()
				restartCmdFlags.postgresql = true
				return restartCmdFlags
			  } (),
			mockRestartCmdHelper: &MockrestartFromBastionImpl{
				getAutomateHAInfraDetailsFunc: func() (*AutomateHAInfraDetails, error) {
					return &AutomateHAInfraDetails{}, nil
				},
				isManagedServicesOnFunc: func() bool {
					return true
				},
				executeRemoteExecutorFunc: func(ntac *NodeTypeAndCmd, s SSHUtil, w *cli.Writer) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, nil
				},
			},
			errorWant: status.Errorf(status.InvalidCommandArgsError, ERROR_ON_MANAGED_SERVICES, POSTGRESQL),
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.description, func(t *testing.T) {
			err := runRestartFromBastion(testCase.flags, testCase.mockRestartCmdHelper)
			if err != nil {
				assert.EqualError(t, testCase.errorWant, err.Error())
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
			errorExepected: nil,
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
