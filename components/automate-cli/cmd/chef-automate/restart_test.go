package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestConstructNodeMapForEachNodeTpe(t *testing.T) {
	var restartCmdFlags = restartCmdFlags{}
	restartCmdFlags.automate = true
	var infra AutomateHAInfraDetails

	type ExpectedOutput struct {
		automate    bool
		chef_server bool
		frontend    bool
	}

	type testCaseInfo struct {
		testCaseDescription string
		Automate            bool
		ChefServer         bool
		frontend            bool
		ExpectedOutput      ExpectedOutput
	}

	testCases := []testCaseInfo{
		{
			testCaseDescription: "should construct nodeMap for automate when automate flag is passed",
			Automate:            true,
			ChefServer:         false,
			frontend:            false,
			ExpectedOutput: ExpectedOutput{
				Automate:    true,
				ChefServer: false,

				frontend:    false,
			},
		},
		{
			testCaseDescription: "should construct nodeMap for chef-server when chef-server flag is passed",
			Automate:            false,
			ChefServer:         true,
			frontend:            false,
			ExpectedOutput: ExpectedOutput{
				Automate:    false,
				ChefServer: true,
				frontend:    false,
			},
		},
		{
			testCaseDescription: "should construct nodeMap for automate when automate flag is passed",
			Automate:            true,
			ChefServer:         false,
			frontend:            false,
			ExpectedOutput: ExpectedOutput{
				automate:    true,
				chef_server: false,
				frontend:    false,
			},
		},
		{
			testCaseDescription: "should construct nodeMap for frontend when frontend flag is passed",
			Automate:            false,
			ChefServer:         false,
			frontend:            true,
			ExpectedOutput: ExpectedOutput{
				automate:    false,
				chef_server: false,
				frontend:    true,
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.testCaseDescription, func(t *testing.T) {
			preflightCmdFlags.automate = tc.Automate
			preflightCmdFlags.chef_server = tc.ChefServer
			preflightCmdFlags.frontend = tc.frontend

			nodeMap := constructAndGetNodeMap(&infra)

			assert.EqualValues(t, nodeMap.Automate.CmdInputs.NodeType, tc.ExpectedOutput.automate)
			assert.EqualValues(t, nodeMap.ChefServer.CmdInputs.NodeType, tc.ExpectedOutput.chef_server)
			assert.EqualValues(t, nodeMap.Frontend.CmdInputs.NodeType, tc.ExpectedOutput.frontend)
		})
	}
}
