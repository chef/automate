package main

import (
	"sync"
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
	printRestartCmdOutputFunc     func(map[string][]*CmdResult, string, *sync.WaitGroup, *sync.Mutex, *cli.Writer)
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

func (mrs *MockrestartFromBastionImpl) printRestartCmdOutput(cmdResult map[string][]*CmdResult, remoteService string, wg *sync.WaitGroup, mutex *sync.Mutex, writer *cli.Writer) {
	mrs.printRestartCmdOutputFunc(cmdResult, remoteService, wg, mutex, writer)
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
						SkipPrintOutput:          true,
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

func TestRunRestartFromBastion(t *testing.T) {
	type testCase struct {
		description         string
		flags               *RestartCmdFlags
		mockStatusCmdHelper *MockrestartFromBastionImpl
		errorWant           error
	}

	testCases := []testCase{
		{
			description: "No service flag provided and node flag is provided",
			flags: &RestartCmdFlags{
				node: "1",
			},
			mockStatusCmdHelper: &MockrestartFromBastionImpl{
				getAutomateHAInfraDetailsFunc: func() (*AutomateHAInfraDetails, error) {
					return &AutomateHAInfraDetails{}, nil
				},
			},
			errorWant: status.Errorf(status.InvalidCommandArgsError, "Please provide service flag"),
		},
		{
			description: "Error while reading infra details",
			flags:       &RestartCmdFlags{},
			mockStatusCmdHelper: &MockrestartFromBastionImpl{
				getAutomateHAInfraDetailsFunc: func() (*AutomateHAInfraDetails, error) {
					return nil, errors.New("Error occured while reading infra details")
				},
			},
			errorWant: errors.New("Error occured while reading infra details"),
		},
		{
			description: "Restart all node-types",
			flags:       &RestartCmdFlags{},
			mockStatusCmdHelper: &MockrestartFromBastionImpl{
				getAutomateHAInfraDetailsFunc: func() (*AutomateHAInfraDetails, error) {
					return &AutomateHAInfraDetails{}, nil
				},
				isManagedServicesOnFunc: func() bool {
					return false
				},
				executeRemoteExecutorFunc: func(ntac *NodeTypeAndCmd, s SSHUtil, w *cli.Writer) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, nil
				},
				printRestartCmdOutputFunc: func(m1 map[string][]*CmdResult, s string, wg *sync.WaitGroup, m2 *sync.Mutex, w *cli.Writer) {
					wg.Done()
				},
			},
			errorWant: nil,
		},
		{
			description: "Error when restarting all node-types with remote execution ",
			flags:       &RestartCmdFlags{},
			mockStatusCmdHelper: &MockrestartFromBastionImpl{
				getAutomateHAInfraDetailsFunc: func() (*AutomateHAInfraDetails, error) {
					return &AutomateHAInfraDetails{}, nil
				},
				isManagedServicesOnFunc: func() bool {
					return false
				},
				executeRemoteExecutorFunc: func(ntac *NodeTypeAndCmd, s SSHUtil, w *cli.Writer) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, errors.New("Some error occured while remote execution")
				},
				printRestartCmdOutputFunc: func(m1 map[string][]*CmdResult, s string, wg *sync.WaitGroup, m2 *sync.Mutex, w *cli.Writer) {
					wg.Done()
				},
			},
			errorWant: errors.New("Some error occured while remote execution"),
		},
		{
			description: "Restarting all services with managed Infra",
			flags:       &RestartCmdFlags{},
			mockStatusCmdHelper: &MockrestartFromBastionImpl{
				getAutomateHAInfraDetailsFunc: func() (*AutomateHAInfraDetails, error) {
					return &AutomateHAInfraDetails{}, nil
				},
				isManagedServicesOnFunc: func() bool {
					return false
				},
				executeRemoteExecutorFunc: func(ntac *NodeTypeAndCmd, s SSHUtil, w *cli.Writer) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, nil
				},
				printRestartCmdOutputFunc: func(m1 map[string][]*CmdResult, s string, wg *sync.WaitGroup, m2 *sync.Mutex, w *cli.Writer) {
					wg.Done()
				},
			},
			errorWant: nil,
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.description, func(t *testing.T) {
			err := runRestartFromBastion(testCase.flags, testCase.mockStatusCmdHelper)
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
		err := handleManagedServiceError(testCase.flags)

		if testCase.errorExepected != nil {
			assert.EqualError(t, err, testCase.errorExepected.Error())
		} else {
			assert.Nil(t, err)
		}
	}
}
