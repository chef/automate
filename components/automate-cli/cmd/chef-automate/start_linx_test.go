package main

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
)

const (
	ERROR    = "Process exited with status 1"
	TEST_IP  = "127.0.1.3"
	TEST_IP2 = "127.0.1.4"
	TEST_IP3 = "127.0.1.5"
)

func TestCheckNodes(t *testing.T) {
	testCases := []struct {
		testName      string
		args          []string
		sshUtilMap    map[string]SSHUtil
		frontendIps   []string
		remoteService string
		isError       bool
		err           error
	}{
		{
			"All th ips are rechable and service restarted",
			[]string{"some_args"},
			createMockSSHUtilMap([]string{TEST_IP, TEST_IP2, TEST_IP3}, nil, "", nil),
			[]string{TEST_IP, TEST_IP2, TEST_IP3},
			"automate",
			false,
			nil,
		},
		{
			"Providing empty agrs for chef server",
			[]string{"some_args"},
			createMockSSHUtilMap(argsEmpty, nil, "", nil),
			argsEmpty,
			"chef-server",
			true,
			status.Errorf(1, "No chef-server IPs are found"),
		},
		{
			"All the ips are not rechable which are provided for chef-server",
			[]string{"some_args"},
			createMockSSHUtilMap([]string{TEST_IP}, nil, "", errors.New("ERROR")),
			[]string{TEST_IP},
			"chef-server",
			true,
			errors.New("Not able to start one or more nodes in chef-server: \nERROR"),
		},
		{
			"All the ips are not rechable which are provided for automate",
			[]string{"some_args"},
			createMockSSHUtilMap([]string{TEST_IP}, nil, "", errors.New("ERROR")),
			[]string{TEST_IP},
			"automate",
			true,
			errors.New("Not able to start one or more nodes in automate: \nERROR"),
		},
		{
			"All th ips are rechable and service started for opensearch",
			[]string{"some_args"},
			createMockSSHUtilMap([]string{TEST_IP, TEST_IP2, TEST_IP3}, nil, "", nil),
			[]string{TEST_IP, TEST_IP2, TEST_IP3},
			"opensearch",
			false,
			nil,
		},
		{
			"Blank Ips are provided for postgres",
			[]string{"some_args"},
			createMockSSHUtilMap([]string{}, nil, "", nil),
			[]string{},
			"postgresql",
			true,
			status.Errorf(1, "No postgresql IPs are found"),
		},
		{
			"Providing all the ips not-rechable agrs for postgres",
			[]string{"some_args"},
			createMockSSHUtilMap([]string{TEST_IP}, nil, "", errors.New("ERROR")),
			[]string{TEST_IP},
			"postgresql",
			true,
			errors.New("Not able to start one or more nodes in postgresql: \nERROR"),
		},
		{
			"Providing all the ips not-rechable agrs for opensearch",
			[]string{"some_args"},
			createMockSSHUtilMap([]string{TEST_IP}, nil, "", errors.New("ERROR")),
			[]string{TEST_IP},
			"opensearch",
			true,
			errors.New("Not able to start one or more nodes in opensearch: \nERROR"),
		},
		{
			"Found output error while starting the hab-sup service for postgres",
			[]string{"some_args"},
			createMockSSHUtilMap([]string{TEST_IP}, nil, "ERROR ,ERROR", nil),
			[]string{TEST_IP},
			"postgresql",
			true,
			errors.New("Not able to start one or more nodes in postgresql: \nERROR ,ERROR"),
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testName, func(t *testing.T) {
			err := checkNodes(testCase.args, testCase.sshUtilMap, testCase.frontendIps, testCase.remoteService, getMockWriterImpl())
			if testCase.isError {
				assert.EqualError(t, err, testCase.err.Error())
			} else {
				assert.Nil(t, err)
			}
		})
	}
}

func TestForRunCommand(t *testing.T) {
	scriptCommand := "sudo chef-automate start"
	testCases := []struct {
		args           []string
		sshUtil        SSHUtil
		scriptCommands string
		output         string
		isError        bool
		err            error
	}{
		{
			[]string{"some_args"},
			getMockSSHUtil(&SSHConfig{}, nil, "Error", nil),
			scriptCommand,
			"Error",
			true,
			errors.New("Error"),
		},
		{
			[]string{"some_args"},
			getMockSSHUtil(&SSHConfig{}, nil, "", errors.New("error")),
			scriptCommand,
			"",
			true,
			errors.New("error"),
		},
		{
			[]string{"some_args"},
			getMockSSHUtil(&SSHConfig{}, nil, "Starting Chef-automate", nil),
			scriptCommand,
			"Starting Chef-automate",
			false,
			nil,
		},
	}
	for _, testCase := range testCases {
		output, err := runCommand(testCase.scriptCommands, testCase.sshUtil)
		if testCase.isError {
			assert.EqualError(t, testCase.err, err.Error())
		} else {
			assert.Equal(t, testCase.output, output)
		}
	}
}

func getMockInfra() *AutomateHAInfraDetails {
	infra := &AutomateHAInfraDetails{}
	infra.Outputs.AutomatePrivateIps.Value = []string{"127.0.0.0"}
	infra.Outputs.ChefServerPrivateIps.Value = []string{"127.0.0.1"}
	infra.Outputs.OpensearchPrivateIps.Value = []string{"127.0.0.2"}
	infra.Outputs.PostgresqlPrivateIps.Value = []string{"127.0.0.4"}
	return infra
}

func createMockSSHUtilMap(ips []string, connectErr error, execOutput string, execErr error) map[string]SSHUtil {
	sshUtilMap := make(map[string]SSHUtil)
	for i := 0; i < len(ips); i++ {
		sshUtilMap[ips[i]] = getMockSSHUtil(&SSHConfig{hostIP: ips[i]}, connectErr, execOutput, execErr)
	}
	return sshUtilMap
}

func TestStartCommandHA(t *testing.T) {
	tests := []struct {
		name            string
		isForAutomate   bool
		isForChefServer bool
		isForPG         bool
		isForOS         bool
		isManaged       bool
		arguments       []string
		isErrorExpected bool
		errorMessage    string
	}{
		{
			name:            "test_Empty_Flags",
			isForAutomate:   false,
			isForChefServer: false,
			isForPG:         false,
			isForOS:         false,
			isManaged:       false,
			arguments:       []string{},
			isErrorExpected: false,
			errorMessage:    "",
		},
		{
			name:            "test_Automate",
			isForAutomate:   true,
			isForChefServer: false,
			isForPG:         false,
			isForOS:         false,
			isManaged:       false,
			arguments:       []string{"--a2"},
			isErrorExpected: true,
			errorMessage:    "\nNot able to start one or more nodes in automate: \nopen : no such file or directory",
		},
		{
			name:            "test_InfraServer",
			isForAutomate:   false,
			isForChefServer: true,
			isForPG:         false,
			isForOS:         false,
			isManaged:       false,
			arguments:       []string{"--cs"},
			isErrorExpected: true,
			errorMessage:    "\nNot able to start one or more nodes in chef-server: \nopen : no such file or directory",
		},
		{
			name:            "test_Postgres",
			isForAutomate:   false,
			isForChefServer: false,
			isForPG:         true,
			isForOS:         false,
			isManaged:       false,
			arguments:       []string{"--pg"},
			isErrorExpected: true,
			errorMessage:    "\nNot able to start one or more nodes in postgresql: \nopen : no such file or directory",
		},
		{
			name:            "test_Postgres_with_ManagedServices",
			isForAutomate:   false,
			isForChefServer: false,
			isForPG:         true,
			isForOS:         false,
			isManaged:       true,
			arguments:       []string{"--pg"},
			isErrorExpected: true,
			errorMessage:    "Starting the service for externally configured postgresql is not supported",
		},
		{
			name:            "test_OpenSearch",
			isForAutomate:   false,
			isForChefServer: false,
			isForPG:         false,
			isForOS:         true,
			isManaged:       false,
			arguments:       []string{"--os"},
			isErrorExpected: true,
			errorMessage:    "\nNot able to start one or more nodes in opensearch: \nopen : no such file or directory",
		},
		{
			name:            "test_OpenSearch_with_ManagedServices",
			isForAutomate:   false,
			isForChefServer: false,
			isForPG:         false,
			isForOS:         true,
			isManaged:       true,
			arguments:       []string{"--os"},
			isErrorExpected: true,
			errorMessage:    "Starting the service for externally configured opensearch is not supported",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			startCmdFlags.automate = tc.isForAutomate
			startCmdFlags.chefServer = tc.isForChefServer
			startCmdFlags.opensearch = tc.isForOS
			startCmdFlags.postgresql = tc.isForPG
			infra := getMockInfra()
			err := runStartCommandHA(infra, tc.arguments, tc.isManaged)
			if tc.isErrorExpected {
				assert.Error(t, err)
				assert.EqualError(t, err, tc.errorMessage)
			} else {
				assert.NoError(t, err)
			}
		})
	}
}
