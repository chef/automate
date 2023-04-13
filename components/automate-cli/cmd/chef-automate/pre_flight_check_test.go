package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

type ExpectedOutput struct {
	SSHKeyFile           string
	SSHPort              string
	SSHUser              string
	AutomatePrivateIps   []string
	ChefServerPrivateIps []string
}

func TestGetInfraDetailsForDeploymentType(t *testing.T) {
	type testCaseInfo struct {
		testCaseDescription string
		deploymentType      string
		configPath          string
		expectedOutput      ExpectedOutput
		expectedError       bool
	}

	testCases := []testCaseInfo{
		{
			testCaseDescription: "should fetch infra details for existing-infra",
			deploymentType:      EXISTING_INFRA_MODE,
			configPath:          "../../pkg/testfiles/onprem/config.toml",
			expectedOutput:      ExpectedOutput{SSHKeyFile: "/home/ec2-user/A2HA.pem", SSHPort: "22", SSHUser: "ec2-user", AutomatePrivateIps: []string{"192.0.2.0", "192.0.2.1"}, ChefServerPrivateIps: []string{"192.0.2.2"}},
			expectedError:       false,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.testCaseDescription, func(t *testing.T) {
			infra, err := getInfraDetailsForDeploymentType(tc.deploymentType, tc.configPath)
			assert.EqualValues(t, infra.Outputs.ChefServerPrivateIps.Value, tc.expectedOutput.ChefServerPrivateIps)
			assert.EqualValues(t, infra.Outputs.AutomatePrivateIps.Value, tc.expectedOutput.AutomatePrivateIps)
			assert.EqualValues(t, infra.Outputs.SSHKeyFile.Value, tc.expectedOutput.SSHKeyFile)
			assert.EqualValues(t, infra.Outputs.SSHPort.Value, tc.expectedOutput.SSHPort)
			assert.EqualValues(t, infra.Outputs.SSHUser.Value, tc.expectedOutput.SSHUser)
			assert.Equal(t, err, nil)
		})
	}
}

func TestConstructAndGetNodeMap(t *testing.T) {
	preflightCmdFlags.automate = true
	var infra AutomateHAInfraDetails

	type ExpectedOutput struct {
		automate    bool
		chef_server bool
		frontend    bool
	}

	type testCaseInfo struct {
		testCaseDescription string
		automate            bool
		chef_server         bool
		frontend            bool
		ExpectedOutput      ExpectedOutput
	}

	testCases := []testCaseInfo{
		{
			testCaseDescription: "should construct nodeMap for automate when automate flag is passed",
			automate:            true,
			chef_server:         false,
			frontend:            false,
			ExpectedOutput: ExpectedOutput{
				automate:    true,
				chef_server: false,
				frontend:    false,
			},
		},
		{
			testCaseDescription: "should construct nodeMap for chef-server when chef-server flag is passed",
			automate:            false,
			chef_server:         true,
			frontend:            false,
			ExpectedOutput: ExpectedOutput{
				automate:    false,
				chef_server: true,
				frontend:    false,
			},
		},
		{
			testCaseDescription: "should construct nodeMap for frontend when frontend flag is passed",
			automate:            false,
			chef_server:         false,
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
			preflightCmdFlags.automate = tc.automate
			preflightCmdFlags.chef_server = tc.chef_server
			preflightCmdFlags.frontend = tc.frontend

			nodeMap := constructAndGetNodeMap(&infra)

			assert.EqualValues(t, nodeMap.Automate.CmdInputs.NodeType, tc.ExpectedOutput.automate)
			assert.EqualValues(t, nodeMap.ChefServer.CmdInputs.NodeType, tc.ExpectedOutput.chef_server)
			assert.EqualValues(t, nodeMap.Frontend.CmdInputs.NodeType, tc.ExpectedOutput.frontend)
		})
	}
}

func TestShouldReturnErrorWhenHaDeploymentConfigFlagDonotHaveConfig(t *testing.T) {
	preflightCmdFlags.haDeploymentConfig = true
	err := runPreflightCheckCmd(nil, nil)
	assert.Equal(t, err.Error(), "Config file should be passed with ha-deployment-config flag")
}

func TestShouldGetInfraDetails(t *testing.T) {
	type testCaseInfo struct {
		testCaseDescription string
		configPath          string
		expectedOutput      ExpectedOutput
		expectedError       bool
	}
	testCases := []testCaseInfo{
		{
			testCaseDescription: "should fetch infra details",
			configPath:          "../../pkg/testfiles/onprem/config.toml",
			expectedOutput:      ExpectedOutput{SSHKeyFile: "/home/ec2-user/A2HA.pem", SSHPort: "22", SSHUser: "ec2-user", AutomatePrivateIps: []string{"192.0.2.0", "192.0.2.1"}, ChefServerPrivateIps: []string{"192.0.2.2"}},
			expectedError:       false,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.testCaseDescription, func(t *testing.T) {
			infra, err := getInfraDetails(tc.configPath)
			assert.EqualValues(t, infra.Outputs.ChefServerPrivateIps.Value, tc.expectedOutput.ChefServerPrivateIps)
			assert.EqualValues(t, infra.Outputs.AutomatePrivateIps.Value, tc.expectedOutput.AutomatePrivateIps)
			assert.EqualValues(t, infra.Outputs.SSHKeyFile.Value, tc.expectedOutput.SSHKeyFile)
			assert.EqualValues(t, infra.Outputs.SSHPort.Value, tc.expectedOutput.SSHPort)
			assert.EqualValues(t, infra.Outputs.SSHUser.Value, tc.expectedOutput.SSHUser)
			assert.Equal(t, err, nil)
		})
	}
}
