package main

import (
	"errors"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/stretchr/testify/assert"
)

func TestBuildFrontEndStatusCmd(t *testing.T) {
	type testCase struct {
		flags           *StatusCmdFlags
		expectedCommand string
	}

	testCases := []testCase{
		{
			flags: &StatusCmdFlags{
				waitRefreshInterval: 2,
				waitTimeout:         600,
			},
			expectedCommand: "sudo chef-automate status -r 2",
		},
		{
			flags: &StatusCmdFlags{
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

func TestHandleManagedServiceForStatusCmd(t *testing.T) {

	testCases := []struct {
		flags          *StatusCmdFlags
		errorExepected error
	}{
		{
			flags: &StatusCmdFlags{
				postgresql: true,
			},
			errorExepected: status.Errorf(status.InvalidCommandArgsError, STATUS_ERROR_ON_SELF_MANAGED, POSTGRESQL),
		},
		{
			flags: &StatusCmdFlags{
				opensearch: true,
			},
			errorExepected: status.Errorf(status.InvalidCommandArgsError, STATUS_ERROR_ON_SELF_MANAGED, OPENSEARCH),
		},
		{
			flags:          &StatusCmdFlags{},
			errorExepected: nil,
		},
		{
			flags: &StatusCmdFlags{
				postgresql: true,
				opensearch: true,
			},
			errorExepected: status.Errorf(status.InvalidCommandArgsError, STATUS_ERROR_ON_SELF_MANAGED, POSTGRESQL+" and "+OPENSEARCH),
		},
	}

	for _, testCase := range testCases {
		err := handleManagedServiceErrorForStatusCmd(testCase.flags)

		if testCase.errorExepected != nil {
			assert.EqualError(t, err, testCase.errorExepected.Error())
		} else {
			assert.Nil(t, err)
		}
	}
}

func TestConstructNodeMapForStatus(t *testing.T) {
	type testCase struct {
		flags           *StatusCmdFlags
		nodeMapExpected *NodeTypeAndCmd
	}
	infra := &AutomateHAInfraDetails{}

	testCases := []testCase{
		{
			flags: &StatusCmdFlags{
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
						Cmd:                      "",
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
						Cmd:                      "",
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
		description       string
		flags             *StatusCmdFlags
		mockNodeOpUtils   *MockNodeUtilsImpl
		mockRemoteCmdExec *MockRemoteCmdExecutor
		errorWant         error
	}

	printStatusOutput := func(m map[string][]*CmdResult, s string, w *cli.Writer) {}

	testCases := []testCase{
		{
			description: "No service flag provided and node flag is provided",
			flags: &StatusCmdFlags{
				node: "1",
			},
			mockNodeOpUtils: &MockNodeUtilsImpl{
				getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
					return &AutomateHAInfraDetails{}, &SSHConfig{}, nil
				},
			},
			errorWant: status.Errorf(status.InvalidCommandArgsError, "Please provide service flag"),
		},
		{
			description: "Error while reading infra details",
			flags:       &StatusCmdFlags{},
			mockNodeOpUtils: &MockNodeUtilsImpl{
				getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
					return nil, nil, errors.New("Error occured while reading infra details")
				},
			},
			errorWant: errors.New("Error occured while reading infra details"),
		},
		{
			description: "Want status of all services",
			flags:       &StatusCmdFlags{},
			mockNodeOpUtils: &MockNodeUtilsImpl{
				getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
					return &AutomateHAInfraDetails{}, &SSHConfig{}, nil
				},
				isManagedServicesOnFunc: func() bool {
					return false
				},
			},
			mockRemoteCmdExec: &MockRemoteCmdExecutor{
				ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, nil
				},
				SetWriterFunc: func(cli *cli.Writer) {},
			},
			errorWant: nil,
		},
		{
			description: "Want status of all services but error occured while remote execution",
			flags:       &StatusCmdFlags{},
			mockNodeOpUtils: &MockNodeUtilsImpl{
				getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
					return &AutomateHAInfraDetails{}, &SSHConfig{}, nil
				},
				isManagedServicesOnFunc: func() bool {
					return false
				},
			},
			mockRemoteCmdExec: &MockRemoteCmdExecutor{
				ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, errors.New("Some error occured while remote execution")
				},
				SetWriterFunc: func(cli *cli.Writer) {},
			},
			errorWant: errors.New("Some error occured while remote execution"),
		},
		{
			description: "Want status of all services with managed services",
			flags:       &StatusCmdFlags{},
			mockNodeOpUtils: &MockNodeUtilsImpl{
				getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
					return &AutomateHAInfraDetails{}, &SSHConfig{}, nil
				},
				isManagedServicesOnFunc: func() bool {
					return false
				},
			},
			mockRemoteCmdExec: &MockRemoteCmdExecutor{
				ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, nil
				},
				SetWriterFunc: func(cli *cli.Writer) {},
			},
			errorWant: nil,
		},
		{
			description: "Managed services and pg flag provided",
			flags: &StatusCmdFlags{
				postgresql: true,
			},
			mockNodeOpUtils: &MockNodeUtilsImpl{
				getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
					return &AutomateHAInfraDetails{}, &SSHConfig{}, nil
				},
				isManagedServicesOnFunc: func() bool {
					return true
				},
			},
			mockRemoteCmdExec: &MockRemoteCmdExecutor{
				ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, nil
				},
				SetWriterFunc: func(cli *cli.Writer) {},
			},
			errorWant: status.Errorf(status.InvalidCommandArgsError, STATUS_ERROR_ON_SELF_MANAGED, POSTGRESQL),
		},
		{
			description: "Want status pg services when --accept-hab-license provided",
			flags: &StatusCmdFlags{
				postgresql:       true,
				acceptHabLicense: true,
			},
			mockNodeOpUtils: &MockNodeUtilsImpl{
				getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
					return &AutomateHAInfraDetails{}, &SSHConfig{}, nil
				},
				isManagedServicesOnFunc: func() bool {
					return false
				},
			},
			mockRemoteCmdExec: &MockRemoteCmdExecutor{
				ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, nil
				},
				SetWriterFunc: func(cli *cli.Writer) {},
			},
			errorWant: nil,
		},
		{
			description: "Want status os services when --accept-hab-license provided",
			flags: &StatusCmdFlags{
				opensearch:       true,
				acceptHabLicense: true,
			},
			mockNodeOpUtils: &MockNodeUtilsImpl{
				getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
					return &AutomateHAInfraDetails{}, &SSHConfig{}, nil
				},
				isManagedServicesOnFunc: func() bool {
					return false
				},
			},
			mockRemoteCmdExec: &MockRemoteCmdExecutor{
				ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, nil
				},
				SetWriterFunc: func(cli *cli.Writer) {},
			},
			errorWant: nil,
		},
		{
			description: "Want status all services when --accept-hab-license provided",
			flags: &StatusCmdFlags{
				acceptHabLicense: true,
			},
			mockNodeOpUtils: &MockNodeUtilsImpl{
				getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
					return &AutomateHAInfraDetails{}, &SSHConfig{}, nil
				},
				isManagedServicesOnFunc: func() bool {
					return false
				},
			},
			mockRemoteCmdExec: &MockRemoteCmdExecutor{
				ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
					return map[string][]*CmdResult{}, nil
				},
				SetWriterFunc: func(cli *cli.Writer) {},
			},
			errorWant: nil,
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.description, func(t *testing.T) {
			err := runStatusFromBastion(testCase.flags, testCase.mockNodeOpUtils, testCase.mockRemoteCmdExec, printStatusOutput)
			if err != nil {
				assert.EqualError(t, testCase.errorWant, err.Error())
			} else {
				assert.Nil(t, err)
			}
		})
	}
}
