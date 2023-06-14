package main

import (
	"sync"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
)

func TestConstructNodeMapForAllNodeTypes(t *testing.T) {
	var restartRestartCmdFlags = RestartCmdFlags{}
	restartRestartCmdFlags.automate = true
	var infra AutomateHAInfraDetails

	type ExpectedOutput struct {
		Automate   bool
		ChefServer bool
		Postgresql bool
		Opensearch bool
	}

	type testCaseInfo struct {
		testCaseDescription string
		Automate            bool
		ChefServer          bool
		Postgresql          bool
		Opensearch          bool
		ExpectedOutput      ExpectedOutput
	}

	testCases := []testCaseInfo{
		{
			testCaseDescription: "should construct nodeMap for frontend when frontend flag is passed",
			Automate:            true,
			ChefServer:          true,
			Postgresql:          true,
			Opensearch:          true,
			ExpectedOutput: ExpectedOutput{
				Automate:   true,
				ChefServer: true,
				Postgresql: true,
				Opensearch: true,
			},
		},
		{
			testCaseDescription: "should construct nodeMap for automate when automate flag is passed",
			Automate:            true,
			ChefServer:          true,
			Postgresql:          true,
			Opensearch:          true,
			ExpectedOutput: ExpectedOutput{
				Automate:   true,
				ChefServer: true,
				Postgresql: true,
				Opensearch: true,
			},
		},
		{
			testCaseDescription: "should construct nodeMap for chef-server when chef-server flag is passed",
			Automate:            true,
			ChefServer:          true,
			Postgresql:          true,
			Opensearch:          true,
			ExpectedOutput: ExpectedOutput{
				Automate:   true,
				ChefServer: true,
				Postgresql: true,
				Opensearch: true,
			},
		},
		{
			testCaseDescription: "should construct nodeMap for postgresql when Postgresql flag is passed",
			Automate:            true,
			ChefServer:          true,
			Postgresql:          true,
			Opensearch:          true,
			ExpectedOutput: ExpectedOutput{
				Automate:   true,
				ChefServer: true,
				Postgresql: true,
				Opensearch: true,
			},
		},
		{
			testCaseDescription: "should construct nodeMap for opensearch when Opensearch flag is passed",
			Automate:            true,
			ChefServer:          true,
			Postgresql:          true,
			Opensearch:          true,
			ExpectedOutput: ExpectedOutput{
				Automate:   true,
				ChefServer: true,
				Postgresql: true,
				Opensearch: true,
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.testCaseDescription, func(t *testing.T) {
			restartRestartCmdFlags.automate = tc.Automate
			restartRestartCmdFlags.chefServer = tc.ChefServer
			restartRestartCmdFlags.postgresql = tc.Postgresql
			restartRestartCmdFlags.opensearch = tc.Opensearch
			nodeMap := constructNodeMapForAllNodeTypes(&restartRestartCmdFlags, &infra)

			assert.EqualValues(t, tc.ExpectedOutput.Automate, nodeMap.Automate.CmdInputs.NodeType)
			assert.EqualValues(t, tc.ExpectedOutput.ChefServer, nodeMap.ChefServer.CmdInputs.NodeType)
			assert.EqualValues(t, tc.ExpectedOutput.Postgresql, nodeMap.Postgresql.CmdInputs.NodeType)
			assert.EqualValues(t, tc.ExpectedOutput.Opensearch, nodeMap.Opensearch.CmdInputs.NodeType)
		})
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
