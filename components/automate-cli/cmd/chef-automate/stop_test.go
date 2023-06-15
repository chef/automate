package main

import (
	"os"
	"testing"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/stretchr/testify/assert"
)

func TestCheckNodeType(t *testing.T) {
	testCases := []struct {
		testName      string
		sshUtilMap    map[string]SSHUtil
		Ips           []string
		remoteService string
		isError       bool
		err           error
	}{
		{
			"All the ips are reachable and stopped the services of automate",
			createMockSSHUtilMap([]string{TEST_IP, TEST_IP2, TEST_IP3}, nil, "", nil),
			[]string{TEST_IP, TEST_IP2, TEST_IP3},
			"automate",
			false,
			nil,
		},
		{
			"Some of the ips are not reachable which are provided for chef-server",
			createMockSSHUtilMap([]string{TEST_IP}, nil, "", errors.New("ERROR")),
			[]string{TEST_IP},
			"chef_server",
			true,
			errors.New("Not able to stop one or more nodes in chef_server: \nERROR"),
		},
		{
			"All th ips are reachable and stopped the services of opensearch",
			createMockSSHUtilMap([]string{TEST_IP, TEST_IP2, TEST_IP3}, nil, "", nil),
			[]string{TEST_IP, TEST_IP2, TEST_IP3},
			"opensearch",
			false,
			nil,
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testName, func(t *testing.T) {
			err := checkNodeType(testCase.sshUtilMap, testCase.Ips, testCase.remoteService)
			if testCase.isError {
				assert.EqualError(t, err, testCase.err.Error())
			} else {
				assert.Nil(t, err)
			}
		})
	}
}

func TestStopCommandHA(t *testing.T) {
	tests := []struct {
		name            string
		isForAutomate   bool
		isForChefServer bool
		isForPG         bool
		isForOS         bool
		isManaged       bool
		isInfraEmpty    bool
		node            string
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
			node:            "",
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
			node:            "",
			isErrorExpected: true,
			errorMessage:    "\nNot able to stop one or more nodes in automate: \nopen : no such file or directory",
		},
		{
			name:            "test_InfraServer",
			isForAutomate:   false,
			isForChefServer: true,
			isForPG:         false,
			isForOS:         false,
			isManaged:       false,
			node:            "",
			isErrorExpected: true,
			errorMessage:    "\nNot able to stop one or more nodes in chef_server: \nopen : no such file or directory",
		},
		{
			name:            "test_Postgres",
			isForAutomate:   false,
			isForChefServer: false,
			isForPG:         true,
			isForOS:         false,
			isManaged:       false,
			node:            "",
			isErrorExpected: true,
			errorMessage:    "\nNot able to stop one or more nodes in postgresql: \nopen : no such file or directory",
		},
		{
			name:            "test_Postgres_with_ManagedServices",
			isForAutomate:   false,
			isForChefServer: false,
			isForPG:         true,
			isForOS:         false,
			isManaged:       true,
			node:            "",
			isErrorExpected: true,
			errorMessage:    "Stopping the service for externally configured postgresql is not supported",
		},
		{
			name:            "test_OpenSearch",
			isForAutomate:   false,
			isForChefServer: false,
			isForPG:         false,
			isForOS:         true,
			isManaged:       false,
			node:            "",
			isErrorExpected: true,
			errorMessage:    "\nNot able to stop one or more nodes in opensearch: \nopen : no such file or directory",
		},
		{
			name:            "test_OpenSearch_with_ManagedServices",
			isForAutomate:   false,
			isForChefServer: false,
			isForPG:         false,
			isForOS:         true,
			isManaged:       true,
			node:            "",
			isErrorExpected: true,
			errorMessage:    "Stopping the service for externally configured opensearch is not supported",
		},
		{
			name:            "test_node_with_MultipleServiceFlags",
			isForAutomate:   true,
			isForChefServer: true,
			isForPG:         false,
			isForOS:         false,
			isManaged:       false,
			node:            "1.2.3.4",
			isErrorExpected: true,
			errorMessage:    "Please remove node flag if you have given multiple service flags.",
		},
		{
			name:            "test_valid_node_with_Automate",
			isForAutomate:   true,
			isForChefServer: false,
			isForPG:         false,
			isForOS:         false,
			isManaged:       false,
			node:            "127.0.0.0",
			isErrorExpected: true,
			errorMessage:    "\nNot able to stop one or more nodes in automate: \nopen : no such file or directory",
		},
		{
			name:            "test_invalid_node_with_Automate",
			isForAutomate:   true,
			isForChefServer: false,
			isForPG:         false,
			isForOS:         false,
			isManaged:       false,
			node:            "127.0.0.5",
			isErrorExpected: true,
			errorMessage:    "\nPlease Enter Valid automate IP",
		},
		{
			name:            "test_empty_infra",
			isForAutomate:   true,
			isForChefServer: false,
			isForPG:         false,
			isForOS:         false,
			isManaged:       false,
			isInfraEmpty:    true,
			node:            "",
			isErrorExpected: true,
			errorMessage:    "\nNo automate IPs are found",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			var infra *AutomateHAInfraDetails
			stopCmdFlags.automate = tc.isForAutomate
			stopCmdFlags.chef_server = tc.isForChefServer
			stopCmdFlags.opensearch = tc.isForOS
			stopCmdFlags.postgresql = tc.isForPG
			stopCmdFlags.node = tc.node
			if tc.isInfraEmpty {
				infra = &AutomateHAInfraDetails{}
			} else {
				infra = getMockInfra()
			}
			err := runStopCommandHA(infra, tc.isManaged)
			if tc.isErrorExpected {
				assert.Error(t, err)
				assert.EqualError(t, err, tc.errorMessage)
			} else {
				assert.NoError(t, err)
			}
		})
	}
}

func TestStopCmd(t *testing.T) {
	tests := []struct {
		testName        string
		isHA            bool
		isDevMode       bool
		isStandalone    bool
		isExpectedError bool
		errorMessage    string
	}{
		{"Dev Mode", false, true, false, true, "Failed to read deployment-service TLS certificates: Could not read the service cert: open /hab/svc/deployment-service/data/deployment-service.crt: no such file or directory"},
		{"Standalone Mode", false, false, true, true, "exec: \"systemctl\": executable file not found in $PATH"},
	}

	for _, tc := range tests {
		t.Run(tc.testName, func(t *testing.T) {
			if tc.isDevMode {
				os.Setenv("CHEF_DEV_ENVIRONMENT", "true")
			} else {
				os.Setenv("CHEF_DEV_ENVIRONMENT", "false")
			}
			err := runStopCmd(&cobra.Command{}, []string{})
			if tc.isExpectedError {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
			}
		})
	}
}

func TestFlagEnabled(t *testing.T) {
	tests := []struct {
		testName        string
		isForAutomate   bool
		isForChefServer bool
		isForPG         bool
		isForOS         bool
		node            string
		isErrorExpected bool
		errorMessage    string
	}{
		{
			testName:        "test_valid_Flags",
			isForAutomate:   true,
			isForChefServer: false,
			isForPG:         false,
			isForOS:         false,
			node:            "",
			isErrorExpected: false,
			errorMessage:    "",
		},
		{
			testName:        "test_empty_service_and_node_flag",
			isForAutomate:   false,
			isForChefServer: false,
			isForPG:         false,
			isForOS:         false,
			node:            "",
			isErrorExpected: true,
			errorMessage:    "No flag is enabled. Please provide any flag",
		},
		{
			testName:        "test_empty_service_but_given_node_flag",
			isForAutomate:   false,
			isForChefServer: false,
			isForPG:         false,
			isForOS:         false,
			node:            "1.2.3.4",
			isErrorExpected: true,
			errorMessage:    "Please provide service flag",
		},
	}

	for _, tc := range tests {
		t.Run(tc.testName, func(t *testing.T) {
			stopCmdFlags.automate = tc.isForAutomate
			stopCmdFlags.chef_server = tc.isForChefServer
			stopCmdFlags.opensearch = tc.isForOS
			stopCmdFlags.postgresql = tc.isForPG
			stopCmdFlags.node = tc.node
			err := isFlagEnabled(&cobra.Command{})
			if tc.isErrorExpected {
				assert.Error(t, err)
				assert.EqualError(t, err, tc.errorMessage)
			} else {
				assert.NoError(t, err)
			}
		})
	}
}
