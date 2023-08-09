package main

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/stretchr/testify/assert"
)

func TestRunServiceVersionsFromBastion(t *testing.T) {
	type testCase struct {
		description       string
		flags             *ServiceVersionsCmdFlags
		mockNodeOpUtils   *MockNodeUtilsImpl
		mockRemoteCmdExec *MockRemoteCmdExecutor
		errorWant         error
	}

	printServiceVersionsOutput := func(m map[string][]*CmdResult, s string, w *cli.Writer) {}
	testCases := []testCase{

		{
			description: "Error while reading infra details",
			flags:       &ServiceVersionsCmdFlags{},
			mockNodeOpUtils: &MockNodeUtilsImpl{
				getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
					return nil, nil, errors.New("Error occured while reading infra details")
				},
			},
			errorWant: errors.New("Error occured while reading infra details"),
		},
		{
			description: "Want service-versions of all services",
			flags: &ServiceVersionsCmdFlags{
				automate:   true,
				chefServer: true,
				opensearch: true,
				postgresql: true,
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
			description: "Want service_versions of backend services",
			flags: &ServiceVersionsCmdFlags{
				automate:   false,
				chefServer: false,
				opensearch: true,
				postgresql: true,
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
			description: "Want service_versions of frontend services",
			flags: &ServiceVersionsCmdFlags{
				automate:   true,
				chefServer: true,
				opensearch: false,
				postgresql: false,
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
			description: "Want service-versions of all services but error occured while remote execution",
			flags: &ServiceVersionsCmdFlags{
				automate:   true,
				chefServer: true,
				opensearch: true,
				postgresql: true,
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
					return map[string][]*CmdResult{}, errors.New("Some error occured while remote execution")
				},
				SetWriterFunc: func(cli *cli.Writer) {},
			},
			errorWant: errors.New("Some error occured while remote execution"),
		},
		{
			description: "Managed services and pg flag provided",

			flags: &ServiceVersionsCmdFlags{
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
			errorWant: status.Errorf(status.InvalidCommandArgsError, SERVICE_VERSIONS_ERROR_ON_SELF_MANAGED, POSTGRESQL),
		},
		{
			description: "Want service-versions of all services but error occured while remote execution",
			flags: &ServiceVersionsCmdFlags{
				automate: true,
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
					cmdResultWithError := &CmdResult{
						ScriptName: "script_1",
						HostIP:     "192.168.1.1",
						Error:      errors.New("Error occurred"),
					}
					return map[string][]*CmdResult{
						"1": {cmdResultWithError},
					}, errors.New("Some error occured while remote execution")
				},
				SetWriterFunc: func(cli *cli.Writer) {},
			},
			errorWant: errors.New("Some error occured while remote execution"),
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.description, func(t *testing.T) {
			err := runServiceVersionsFromBastion(testCase.flags, testCase.mockNodeOpUtils, testCase.mockRemoteCmdExec, printServiceVersionsOutput)
			if err != nil {
				assert.EqualError(t, testCase.errorWant, err.Error())
			} else {
				assert.Nil(t, err)
			}
		})
	}
}

func TestHandleManagedServiceForServiceVersionCmd(t *testing.T) {

	testCases := []struct {
		flags          *ServiceVersionsCmdFlags
		errorExepected error
	}{
		{
			flags: &ServiceVersionsCmdFlags{

				postgresql: true,
			},
			errorExepected: status.Errorf(status.InvalidCommandArgsError, SERVICE_VERSIONS_ERROR_ON_SELF_MANAGED, POSTGRESQL),
		},
		{
			flags: &ServiceVersionsCmdFlags{

				opensearch: true,
			},
			errorExepected: status.Errorf(status.InvalidCommandArgsError, SERVICE_VERSIONS_ERROR_ON_SELF_MANAGED, OPENSEARCH),
		},
		{
			flags: &ServiceVersionsCmdFlags{},

			errorExepected: nil,
		},
		{
			flags: &ServiceVersionsCmdFlags{
				opensearch: true,
				postgresql: true,
			},
			errorExepected: status.Errorf(status.InvalidCommandArgsError, SERVICE_VERSIONS_ERROR_ON_SELF_MANAGED, POSTGRESQL+" and "+OPENSEARCH),
		},
	}

	for _, testCase := range testCases {
		err := handleManagedServiceErrorForServiceVersionsCmd(testCase.flags)

		if testCase.errorExepected != nil {
			assert.EqualError(t, err, testCase.errorExepected.Error())
		} else {
			assert.Nil(t, err)
		}
	}
}

func TestConstructNodeMapForServiceVersions(t *testing.T) {
	type testCase struct {
		flags           *ServiceVersionsCmdFlags
		nodeMapExpected *NodeTypeAndCmd
	}
	infra := &AutomateHAInfraDetails{}

	testCases := []testCase{
		{
			flags: &ServiceVersionsCmdFlags{
				automate:   true,
				chefServer: true,
			},
			nodeMapExpected: &NodeTypeAndCmd{
				Frontend: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:                      FRONTEND_SERVICE_VERSIONS_CMD,
						SkipPrintOutput:          true,
						HideSSHConnectionMessage: true,
					},
				},
				Automate: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:                      FRONTEND_SERVICE_VERSIONS_CMD,
						ErrorCheckEnableInOutput: true,
						NodeIps:                  []string{""},
						NodeType:                 true,
						SkipPrintOutput:          true,
						HideSSHConnectionMessage: true,
					},
				},
				ChefServer: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:                      FRONTEND_SERVICE_VERSIONS_CMD,
						ErrorCheckEnableInOutput: true,
						NodeIps:                  []string{""},
						NodeType:                 true,
						SkipPrintOutput:          true,
						HideSSHConnectionMessage: true,
					},
				},
				Postgresql: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:                      BACKEND_SERVICE_VERSIONS_CMD,
						NodeIps:                  []string{""},
						ErrorCheckEnableInOutput: true,
						NodeType:                 false,
						SkipPrintOutput:          true,
						HideSSHConnectionMessage: true,
					},
				},
				Opensearch: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:                      BACKEND_SERVICE_VERSIONS_CMD,
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
		nodeMapGet := constructNodeMapForServiceVersions(infra, testCase.flags)
		assert.Equal(t, testCase.nodeMapExpected, nodeMapGet)
	}
}

func TestRunServiceVersionsCmd(t *testing.T) {
	tests := []struct {
		testName        string
		isHA            bool
		isDevMode       bool
		isStandalone    bool
		isExpectedError bool
		errorMessage    string
		mockNodeOpUtils *MockNodeUtilsImpl
	}{
		{
			mockNodeOpUtils: &MockNodeUtilsImpl{
				isA2HARBFileExistFunc: func() bool {
					return true
				},
			},
			isExpectedError: true,
			errorMessage:    "Failed to read deployment-service TLS certificates: Could not read the service cert: open /hab/svc/deployment-service/data/deployment-service.crt: no such file or directory",
		},
	}

	for _, tc := range tests {
		t.Run(tc.testName, func(t *testing.T) {
			err := runServiceVersionsCmd(&cobra.Command{}, []string{})
			if tc.isExpectedError {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
			}
		})
	}
}

func TestIsFalgSet(t *testing.T) {

	testCases := []struct {
		flags          *ServiceVersionsCmdFlags
		errorExepected error
	}{
		{
			flags:          &ServiceVersionsCmdFlags{},
			errorExepected: errors.New("No flag is enabled. Please provide any flag"),
		},
		{
			flags: &ServiceVersionsCmdFlags{
				node: "12",
			},
			errorExepected: status.Errorf(status.InvalidCommandArgsError, "Please provide service flag"),
		},
		{

			flags: &ServiceVersionsCmdFlags{
				node:     "12",
				automate: true,
			},
			errorExepected: nil,
		},
	}

	for _, testCases := range testCases {
		err := isFlagSet(&cobra.Command{}, testCases.flags)

		if testCases.errorExepected != nil {
			assert.EqualError(t, err, testCases.errorExepected.Error())
		} else {
			assert.Nil(t, err)
		}
	}
}
