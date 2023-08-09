package main

import (
	"testing"

	"github.com/spf13/cobra"
	"github.com/stretchr/testify/assert"
)

func TestInternalCaHA(t *testing.T) {
	tests := []struct {
		name            string
		isForAutomate   bool
		isForChefServer bool
		isInfraEmpty    bool
		node            string
		command         string
		isErrorExpected bool
		errorMessage    string
	}{
		{
			name:            "test_Automate",
			isForAutomate:   true,
			isForChefServer: false,
			node:            "",
			isErrorExpected: true,
			errorMessage:    "Not able to generate root or fetch root info: \nopen : no such file or directory",
		},
		{
			name:            "test_ChefServer",
			isForAutomate:   false,
			isForChefServer: true,
			node:            "",
			isErrorExpected: true,
			errorMessage:    "Not able to generate root or fetch root info: \nopen : no such file or directory",
		},
		{
			name:            "test_node_with_MultipleServiceFlags",
			isForAutomate:   true,
			isForChefServer: true,
			node:            "1.2.3.4",
			isErrorExpected: true,
			errorMessage:    "Please remove node flag if you have given multiple service flags.",
		},
		{
			name:            "test_valid_node_with_Automate",
			isForAutomate:   true,
			isForChefServer: false,
			node:            "127.0.0.0",
			isErrorExpected: true,
			errorMessage:    "Not able to generate root or fetch root info: \nopen : no such file or directory",
		},
		{
			name:            "test_invalid_node_with_Automate",
			isForAutomate:   true,
			isForChefServer: false,
			node:            "127.0.0.5",
			isErrorExpected: true,
			errorMessage:    "Please Enter Valid automate IP",
		},
		{
			name:            "test_valid_node_with_ChefServer",
			isForAutomate:   false,
			isForChefServer: true,
			node:            "127.0.0.1",
			isErrorExpected: true,
			errorMessage:    "Not able to generate root or fetch root info: \nopen : no such file or directory",
		},
		{
			name:            "test_invalid_node_with_ChefServer",
			isForAutomate:   false,
			isForChefServer: true,
			node:            "127.0.0.5",
			isErrorExpected: true,
			errorMessage:    "Please Enter Valid chef_server IP",
		},
		{
			name:            "test_empty_infra",
			isForAutomate:   true,
			isForChefServer: false,
			isInfraEmpty:    true,
			node:            "",
			isErrorExpected: true,
			errorMessage:    "No automate IPs are found",
		},
		{
			name:            "test_multiple_service_flags",
			isForAutomate:   true,
			isForChefServer: true,
			node:            "",
			isErrorExpected: true,
			errorMessage:    "Not able to generate root or fetch root info: \nopen : no such file or directory\nopen : no such file or directory",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			var infra *AutomateHAInfraDetails
			caCmdFlags.automate = tc.isForAutomate
			caCmdFlags.chef_server = tc.isForChefServer
			caCmdFlags.node = tc.node
			if tc.isInfraEmpty {
				infra = &AutomateHAInfraDetails{}
			} else {
				infra = getMockInfra()
			}
			err := runInternalCaHA(infra, tc.command)
			if tc.isErrorExpected {
				assert.Error(t, err)
				assert.EqualError(t, err, tc.errorMessage)
			} else {
				assert.NoError(t, err)
			}
		})
	}
}

func TestFeFlagEnabled(t *testing.T) {
	tests := []struct {
		testName        string
		isForAutomate   bool
		isForChefServer bool
		node            string
		isErrorExpected bool
		errorMessage    string
	}{
		{
			testName:        "test_valid_Flags",
			isForAutomate:   true,
			isForChefServer: false,
			node:            "",
			isErrorExpected: false,
			errorMessage:    "",
		},
		{
			testName:        "test_empty_service_and_node_flag",
			isForAutomate:   false,
			isForChefServer: false,
			node:            "",
			isErrorExpected: true,
			errorMessage:    "No flag is enabled. Please provide any flag",
		},
		{
			testName:        "test_empty_service_but_given_node_flag",
			isForAutomate:   false,
			isForChefServer: false,
			node:            "1.2.3.4",
			isErrorExpected: true,
			errorMessage:    "Please provide service flag",
		},
	}

	for _, tc := range tests {
		t.Run(tc.testName, func(t *testing.T) {
			caCmdFlags.automate = tc.isForAutomate
			caCmdFlags.chef_server = tc.isForChefServer
			caCmdFlags.node = tc.node
			err := isFeFlagEnabled(&cobra.Command{})
			if tc.isErrorExpected {
				assert.Error(t, err)
				assert.EqualError(t, err, tc.errorMessage)
			} else {
				assert.NoError(t, err)
			}
		})
	}
}
