package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestConstructNodeMapForEachNodeTpe(t *testing.T) {
	restartCmdFlags.automate = true
	var infra AutomateHAInfraDetails

	type ExpectedOutput struct {
		Automate   bool
		ChefServer bool
		Postgresql bool
		Opensearch bool
		frontend   bool
	}

	type testCaseInfo struct {
		testCaseDescription string
		Automate            bool
		ChefServer          bool
		Postgresql          bool
		Opensearch          bool
		frontend            bool
		ExpectedOutput      ExpectedOutput
	}

	testCases := []testCaseInfo{
		{
			testCaseDescription: "should construct nodeMap for frontend when frontend flag is passed",
			Automate:            true,
			ChefServer:          true,
			Postgresql:          true,
			Opensearch:          true,
			frontend:            true,
			ExpectedOutput: ExpectedOutput{
				Automate:   true,
				ChefServer: true,
				Postgresql: true,
				Opensearch: true,
				frontend:   true,
			},
		},
		{
			testCaseDescription: "should construct nodeMap for automate when automate flag is passed",
			Automate:            true,
			ChefServer:          true,
			Postgresql:          true,
			Opensearch:          true,
			frontend:            true,
			ExpectedOutput: ExpectedOutput{
				Automate:   true,
				ChefServer: true,
				Postgresql: true,
				Opensearch: true,
				frontend:   true,
			},
		},
		{
			testCaseDescription: "should construct nodeMap for chef-server when chef-server flag is passed",
			Automate:            true,
			ChefServer:          true,
			Postgresql:          true,
			Opensearch:          true,
			frontend:            true,
			ExpectedOutput: ExpectedOutput{
				Automate:   true,
				ChefServer: true,
				Postgresql: true,
				Opensearch: true,
				frontend:   true,
			},
		},
		{
			testCaseDescription: "should construct nodeMap for postgresql when Postgresql flag is passed",
			Automate:            true,
			ChefServer:          true,
			Postgresql:          true,
			Opensearch:          true,
			frontend:            true,
			ExpectedOutput: ExpectedOutput{
				Automate:   true,
				ChefServer: true,
				Postgresql: true,
				Opensearch: true,
				frontend:   true,
			},
		},
		{
			testCaseDescription: "should construct nodeMap for opensearch when Opensearch flag is passed",
			Automate:            true,
			ChefServer:          true,
			Postgresql:          true,
			Opensearch:          true,
			frontend:            true,
			ExpectedOutput: ExpectedOutput{
				Automate:   true,
				ChefServer: true,
				Postgresql: true,
				Opensearch: true,
				frontend:   true,
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.testCaseDescription, func(t *testing.T) {
			restartCmdFlags.automate = tc.Automate
			restartCmdFlags.chefServer = tc.ChefServer
			restartCmdFlags.postgresql = tc.Postgresql
			restartCmdFlags.opensearch = tc.Opensearch
			nodeMap := ConstructNodeMapForEachNodeTpe(&infra,&restartCmdFlags)

			assert.EqualValues(t, tc.ExpectedOutput.Automate, nodeMap.Automate.CmdInputs.NodeType)
			assert.EqualValues(t, tc.ExpectedOutput.ChefServer, nodeMap.ChefServer.CmdInputs.NodeType)
			assert.EqualValues(t, tc.ExpectedOutput.Postgresql, nodeMap.Postgresql.CmdInputs.NodeType)
			assert.EqualValues(t, tc.ExpectedOutput.Opensearch, nodeMap.Opensearch.CmdInputs.NodeType)
		})
	}
}

func TestConstructNodeMapForAllNodeTypes(t *testing.T) {
	restartCmdFlags.automate = true
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
			restartCmdFlags.automate = tc.Automate
			restartCmdFlags.chefServer = tc.ChefServer
			restartCmdFlags.postgresql = tc.Postgresql
			restartCmdFlags.opensearch = tc.Opensearch
			nodeMap := constructNodeMapForAllNodeTypes(&restartCmdFlags,&infra)

			assert.EqualValues(t, tc.ExpectedOutput.Automate, nodeMap.Automate.CmdInputs.NodeType)
			assert.EqualValues(t, tc.ExpectedOutput.ChefServer, nodeMap.ChefServer.CmdInputs.NodeType)
			assert.EqualValues(t, tc.ExpectedOutput.Postgresql, nodeMap.Postgresql.CmdInputs.NodeType)
			assert.EqualValues(t, tc.ExpectedOutput.Opensearch, nodeMap.Opensearch.CmdInputs.NodeType)
		})
	}
}
