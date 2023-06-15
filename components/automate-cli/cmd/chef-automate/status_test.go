package main

import (
	"errors"
	"sync"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/stretchr/testify/assert"
)

type MockStatusCmdFromBastionHelperImpl struct {
	getAutomateHAInfraDetailsFunc func() (*AutomateHAInfraDetails, error)
	isManagedServicesOnFunc       func() bool
	executeRemoteExecutorFunc     func(*NodeTypeAndCmd, SSHUtil, *cli.Writer) (map[string][]*CmdResult, error)
	printStatusOutputFunc         func(map[string][]*CmdResult, string, *sync.Mutex, *cli.Writer)
}

func (mst *MockStatusCmdFromBastionHelperImpl) getAutomateHAInfraDetails() (*AutomateHAInfraDetails, error) {
	return mst.getAutomateHAInfraDetailsFunc()
}

func (mst *MockStatusCmdFromBastionHelperImpl) isManagedServicesOn() bool {
	return mst.isManagedServicesOnFunc()
}

func (mst *MockStatusCmdFromBastionHelperImpl) executeRemoteExecutor(nodemap *NodeTypeAndCmd, sshUtil SSHUtil, writer *cli.Writer) (map[string][]*CmdResult, error) {
	return mst.executeRemoteExecutorFunc(nodemap, sshUtil, writer)
}

func (mst *MockStatusCmdFromBastionHelperImpl) printStatusOutput(cmdResult map[string][]*CmdResult, remoteService string, mutex *sync.Mutex, writer *cli.Writer) {
	mst.printStatusOutputFunc(cmdResult, remoteService, mutex, writer)
}

func TestBuildFrontEndStatusCmd(t *testing.T) {
	type testCase struct {
		flags           *statusCmdFlags
		expectedCommand string
	}

	testCases := []testCase{
		{
			flags: &statusCmdFlags{
				waitRefreshInterval: 2,
				waitTimeout:         600,
			},
			expectedCommand: "sudo chef-automate status -r 2",
		},
		{
			flags: &statusCmdFlags{
				waitRefreshInterval: 2,
				waitTimeout:         600,
				waitForHealthy:      true,
			},
			expectedCommand: "sudo chef-automate status -w -t 600 -r 2",
		},
	}

	for _, testCase := range testCases {
		t.Run("Checking command creation", func(t *testing.T) {
			commandGet := buildFrontEndStatusCmd(testCase.flags)
			assert.Equal(t, testCase.expectedCommand, commandGet)
		})
	}
}

func TestHandleManagedServiceError(t *testing.T) {

	testCases := []struct {
		flags          *statusCmdFlags
		errorExepected error
	}{
		{
			flags: &statusCmdFlags{
				postgresql: true,
			},
			errorExepected: status.Errorf(status.InvalidCommandArgsError, STATUS_ERROR_ON_SELF_MANAGED, POSTGRESQL),
		},
		{
			flags: &statusCmdFlags{
				opensearch: true,
			},
			errorExepected: status.Errorf(status.InvalidCommandArgsError, STATUS_ERROR_ON_SELF_MANAGED, OPENSEARCH),
		},
		{
			flags:          &statusCmdFlags{},
			errorExepected: nil,
		},
		{
			flags: &statusCmdFlags{
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

func TestConstructNodeMapForStatus(t *testing.T) {
	type testCase struct {
		flags           *statusCmdFlags
		nodeMapExpected *NodeTypeAndCmd
	}
	infra := &AutomateHAInfraDetails{}

	testCases := []testCase{
		{
			flags: &statusCmdFlags{
				waitForHealthy:      false,
				waitTimeout:         600,
				waitRefreshInterval: 10,
				automate:            true,
			},
			nodeMapExpected: &NodeTypeAndCmd{
				Frontend: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:                      "sudo chef-automate status -r 10",
						WaitTimeout:              600,
						SkipPrintOutput:          true,
						HideSSHConnectionMessage: true,
					},
				},
				Automate: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:                      "sudo chef-automate status -r 10",
						WaitTimeout:              600,
						ErrorCheckEnableInOutput: true,
						NodeIps:                  []string{""},
						NodeType:                 true,
						SkipPrintOutput:          true,
						HideSSHConnectionMessage: true,
					},
				},
				ChefServer: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:                      "sudo chef-automate status -r 10",
						WaitTimeout:              600,
						ErrorCheckEnableInOutput: true,
						NodeIps:                  []string{""},
						NodeType:                 false,
						SkipPrintOutput:          true,
						HideSSHConnectionMessage: true,
					},
				},
				Postgresql: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:                      BACKEND_STATUS,
						NodeIps:                  []string{""},
						ErrorCheckEnableInOutput: true,
						NodeType:                 false,
						WaitTimeout:              600,
						SkipPrintOutput:          true,
						HideSSHConnectionMessage: true,
					},
				},
				Opensearch: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:                      BACKEND_STATUS,
						NodeIps:                  []string{""},
						ErrorCheckEnableInOutput: true,
						NodeType:                 false,
						WaitTimeout:              600,
						SkipPrintOutput:          true,
						HideSSHConnectionMessage: true,
					},
				},
				Infra: infra,
			},
		},
	}

	for _, testCase := range testCases {
		nodeMapGet := constructNodeMapForStatus(testCase.flags, infra)
		assert.Equal(t, testCase.nodeMapExpected, nodeMapGet)
	}
}

func TestRunStatusFromBastion(t *testing.T) {
	type testCase struct {
		description         string
		flags               *statusCmdFlags
		mockStatusCmdHelper *MockStatusCmdFromBastionHelperImpl
		errorWant           error
	}

	testCases := []testCase{
		{
			description: "No service flag provided and node flag is provided",
			flags: &statusCmdFlags{
				node: "1",
			},
			mockStatusCmdHelper: &MockStatusCmdFromBastionHelperImpl{
				getAutomateHAInfraDetailsFunc: func() (*AutomateHAInfraDetails, error) {
					return &AutomateHAInfraDetails{}, nil
				},
			},
			errorWant: status.Errorf(status.InvalidCommandArgsError, "Please provide service flag"),
		},
		{
			description: "Error while reading infra details",
			flags:       &statusCmdFlags{},
			mockStatusCmdHelper: &MockStatusCmdFromBastionHelperImpl{
				getAutomateHAInfraDetailsFunc: func() (*AutomateHAInfraDetails, error) {
					return nil, errors.New("Error occured while reading infra details")
				},
			},
			errorWant: errors.New("Error occured while reading infra details"),
		},
		{
			description: "Want status of all services",
			flags:       &statusCmdFlags{},
			mockStatusCmdHelper: &MockStatusCmdFromBastionHelperImpl{
				getAutomateHAInfraDetailsFunc: func() (*AutomateHAInfraDetails, error) {
					return &AutomateHAInfraDetails{}, nil
				},
				isManagedServicesOnFunc: func() bool {
					return false
				},
				executeRemoteExecutorFunc: func(ntac *NodeTypeAndCmd, s SSHUtil, w *cli.Writer) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, nil
				},
				printStatusOutputFunc: func(m1 map[string][]*CmdResult, s string, mtx *sync.Mutex, w *cli.Writer) {
				},
			},
			errorWant: nil,
		},
		{
			description: "Want status of all services but error occured while remote execution",
			flags:       &statusCmdFlags{},
			mockStatusCmdHelper: &MockStatusCmdFromBastionHelperImpl{
				getAutomateHAInfraDetailsFunc: func() (*AutomateHAInfraDetails, error) {
					return &AutomateHAInfraDetails{}, nil
				},
				isManagedServicesOnFunc: func() bool {
					return false
				},
				executeRemoteExecutorFunc: func(ntac *NodeTypeAndCmd, s SSHUtil, w *cli.Writer) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, errors.New("Some error occured while remote execution")
				},
				printStatusOutputFunc: func(m1 map[string][]*CmdResult, s string, mtx *sync.Mutex, w *cli.Writer) {
				},
			},
			errorWant: errors.New("Some error occured while remote execution"),
		},
		{
			description: "Want status of all services with managed services",
			flags:       &statusCmdFlags{},
			mockStatusCmdHelper: &MockStatusCmdFromBastionHelperImpl{
				getAutomateHAInfraDetailsFunc: func() (*AutomateHAInfraDetails, error) {
					return &AutomateHAInfraDetails{}, nil
				},
				isManagedServicesOnFunc: func() bool {
					return true
				},
				executeRemoteExecutorFunc: func(ntac *NodeTypeAndCmd, s SSHUtil, w *cli.Writer) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, nil
				},
				printStatusOutputFunc: func(m1 map[string][]*CmdResult, s string, mtx *sync.Mutex, w *cli.Writer) {
				},
			},
			errorWant: nil,
		},
		{
			description: "Managed services and pg flag provided",
			flags: &statusCmdFlags{
				postgresql: true,
			},
			mockStatusCmdHelper: &MockStatusCmdFromBastionHelperImpl{
				getAutomateHAInfraDetailsFunc: func() (*AutomateHAInfraDetails, error) {
					return &AutomateHAInfraDetails{}, nil
				},
				isManagedServicesOnFunc: func() bool {
					return true
				},
				executeRemoteExecutorFunc: func(ntac *NodeTypeAndCmd, s SSHUtil, w *cli.Writer) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, nil
				},
				printStatusOutputFunc: func(m1 map[string][]*CmdResult, s string, mtx *sync.Mutex, w *cli.Writer) {
				},
			},
			errorWant: status.Errorf(status.InvalidCommandArgsError, STATUS_ERROR_ON_SELF_MANAGED, POSTGRESQL),
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.description, func(t *testing.T) {
			err := runStatusFromBastion(testCase.flags, testCase.mockStatusCmdHelper)
			if err != nil {
				assert.EqualError(t, testCase.errorWant, err.Error())
			} else {
				assert.Nil(t, err)
			}
		})
	}
}
