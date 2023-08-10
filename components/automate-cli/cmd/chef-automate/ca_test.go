package main

import (
	"os"
	"runtime"
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
	// Resetting the global variables to its default values.
	caCmdFlags.automate = false
	caCmdFlags.chef_server = false
	caCmdFlags.node = ""
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
	// Resetting the global variables to its default values.
	caCmdFlags.automate = false
	caCmdFlags.chef_server = false
	caCmdFlags.node = ""
}

func TestCAInfo(t *testing.T) {
	tests := []struct {
		testName        string
		isHA            bool
		isStandalone    bool
		isFeFlagEnabled bool
		isErrorExpected bool
		errorMessage    string
	}{
		{
			testName:        "Standalone_Mode",
			isHA:            false,
			isStandalone:    true,
			isFeFlagEnabled: false,
			isErrorExpected: true,
			errorMessage:    "Connecting to deployment-service failed: Failed to read deployment-service TLS certificates: Could not read the service cert: open /hab/svc/deployment-service/data/deployment-service.crt: no such file or directory",
		},
		{
			testName:        "HA_Mode",
			isHA:            true,
			isStandalone:    false,
			isFeFlagEnabled: false,
			isErrorExpected: true,
			errorMessage:    "No flag is enabled. Please provide any flag",
		},
		{
			testName:        "HA_Mode_with_Flag_Enabled",
			isHA:            true,
			isStandalone:    false,
			isFeFlagEnabled: true,
			isErrorExpected: true,
			errorMessage:    "Automate Ha infra confile file not exist",
		},
	}

	for _, tc := range tests {
		t.Run(tc.testName, func(t *testing.T) {
			if tc.isHA {
				if runtime.GOOS == "darwin" {
					return
				}
				caCmdFlags.automate = tc.isFeFlagEnabled
				defer os.Remove(CreateHASystem(t).Name())
			}
			err := runCAInfoCmd(&cobra.Command{}, []string{})
			if tc.isErrorExpected {
				assert.Error(t, err)
				assert.EqualError(t, err, tc.errorMessage)
			} else {
				assert.NoError(t, err)
			}
		})
	}
	// Resetting the global variables to its default values.
	caCmdFlags.automate = false
}

func TestRegenRoot(t *testing.T) {
	tests := []struct {
		testName        string
		isHA            bool
		isStandalone    bool
		isFeFlagEnabled bool
		isErrorExpected bool
		errorMessage    string
	}{
		{
			testName:        "Standalone_Mode",
			isHA:            false,
			isStandalone:    true,
			isFeFlagEnabled: false,
			isErrorExpected: true,
			errorMessage:    "Connecting to deployment-service failed: Failed to read deployment-service TLS certificates: Could not read the service cert: open /hab/svc/deployment-service/data/deployment-service.crt: no such file or directory",
		},
		{
			testName:        "HA_Mode",
			isHA:            true,
			isStandalone:    false,
			isFeFlagEnabled: false,
			isErrorExpected: true,
			errorMessage:    "No flag is enabled. Please provide any flag",
		},
		{
			testName:        "HA_Mode_with_Flag_Enabled",
			isHA:            true,
			isStandalone:    false,
			isFeFlagEnabled: true,
			isErrorExpected: true,
			errorMessage:    "Automate Ha infra confile file not exist",
		},
	}

	for _, tc := range tests {
		t.Run(tc.testName, func(t *testing.T) {
			if tc.isHA {
				if runtime.GOOS == "darwin" {
					return
				}
				caCmdFlags.automate = tc.isFeFlagEnabled
				defer os.Remove(CreateHASystem(t).Name())
			}
			err := runRegenRootCmd(&cobra.Command{}, []string{})
			if tc.isErrorExpected {
				assert.Error(t, err)
				assert.ErrorContains(t, err, tc.errorMessage)
			} else {
				assert.NoError(t, err)
			}
		})
	}
	// Resetting the global variables to its default values.
	caCmdFlags.automate = false
}

func CreateHASystem(t *testing.T) *os.File {
	dirPath := initConfigHabA2HAPathFlag.a2haDirPath
	filePath := dirPath + "/a2ha.rb"
	err := os.MkdirAll(dirPath, os.ModePerm)
	assert.NoError(t, err, "Error creating directories")
	file, err := os.Create(filePath)
	assert.NoError(t, err, "Error creating file")
	return file
}
