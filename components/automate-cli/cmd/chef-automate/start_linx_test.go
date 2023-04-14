package main

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
)

func TestStartForFrontEndNodes(t *testing.T) {
	testCases := []struct {
		args          []string
		sshUtilMap    map[string]SSHUtil
		frontendIps   []string
		remoteService string
		isError       bool
		err           error
	}{
		{
			[]string{"some_args"},
			createMockSSHUtilMap([]string{"127.0.1.3", "127.0.1.4", "127.0.1.5"}, nil, "", nil),
			[]string{"127.0.1.3", "127.0.1.4", "127.0.1.5"},
			"automate",
			false,
			nil,
		},
		{
			[]string{"some_args"},
			createMockSSHUtilMap(argsEmpty, nil, "", nil),
			argsEmpty,
			"chef-server",
			true,
			status.Errorf(1, "No chef-server IPs are found"),
		},
		{
			[]string{"some_args"},
			createMockSSHUtilMap([]string{"127.0.1.3"}, nil, "", errors.New("Process exited with status 1")),
			[]string{"127.0.1.3"},
			"chef-server",
			true,
			errors.New("Not able to start one or more nodes in chef-server: \nProcess exited with status 1"),
		},
	}

	for _, testCase := range testCases {
		err := checkNodes(testCase.args, testCase.sshUtilMap, testCase.frontendIps, testCase.remoteService, getMockWriterImpl())
		if testCase.isError {
			assert.EqualError(t, testCase.err, err.Error())
		} else {
			assert.Nil(t, err)
		}
	}
}

func TestStartForBackEndNodes(t *testing.T) {
	testCases := []struct {
		args          []string
		sshUtilMap    map[string]SSHUtil
		frontendIps   []string
		remoteService string
		isError       bool
		err           error
	}{
		{
			[]string{"some_args"},
			createMockSSHUtilMap([]string{"127.0.1.3", "127.0.1.4", "127.0.1.5"}, nil, "", nil),
			[]string{"127.0.1.3", "127.0.1.4", "127.0.1.5"},
			"opensearch",
			false,
			nil,
		},
		{
			[]string{"some_args"},
			createMockSSHUtilMap([]string{}, nil, "", nil),
			[]string{},
			"postgresql",
			true,
			status.Errorf(1, "No postgresql IPs are found"),
		},
		{
			[]string{"some_args"},
			createMockSSHUtilMap([]string{"127.0.1.3"}, nil, "", errors.New("Process exited with status 1")),
			[]string{"127.0.1.3"},
			"postgresql",
			true,
			errors.New("Not able to start one or more nodes in postgresql: \nProcess exited with status 1"),
		},
	}

	for _, testCase := range testCases {
		err := checkNodes(testCase.args, testCase.sshUtilMap, testCase.frontendIps, testCase.remoteService, getMockWriterImpl())
		if testCase.isError {
			assert.EqualError(t, testCase.err, err.Error())
		} else {
			assert.Nil(t, err)
		}
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

func TestFunc(t *testing.T) {
	startCmdFlags.chefServer = true
	startCmdFlags.automate = false
	startCmdFlags.opensearch = false
	startCmdFlags.postgresql = false
	infra := getMockInfra()
	err := runStartCommandHA(infra, []string{"--cs"})
	assert.EqualError(t, err, "\nNot able to start one or more nodes in chef-server: \nopen : no such file or directory")
}

func TestFuncAutomate(t *testing.T) {
	startCmdFlags.chefServer = false
	startCmdFlags.automate = true
	startCmdFlags.opensearch = false
	startCmdFlags.postgresql = false
	infra := getMockInfra()
	err := runStartCommandHA(infra, []string{"--a2"})
	assert.EqualError(t, err, "\nNot able to start one or more nodes in automate: \nopen : no such file or directory")
}

func TestFuncOs(t *testing.T) {
	startCmdFlags.chefServer = false
	startCmdFlags.automate = false
	startCmdFlags.opensearch = true
	startCmdFlags.postgresql = false
	infra := getMockInfra()
	err := runStartCommandHA(infra, []string{"--os"})
	assert.EqualError(t, err, "\nNot able to start one or more nodes in opensearch: \nopen : no such file or directory")
}
func TestFuncPg(t *testing.T) {
	startCmdFlags.chefServer = false
	startCmdFlags.automate = false
	startCmdFlags.opensearch = false
	startCmdFlags.postgresql = true
	infra := getMockInfra()
	err := runStartCommandHA(infra, []string{"--pg"})
	assert.EqualError(t, err, "\nNot able to start one or more nodes in postgresql: \nopen : no such file or directory")
}

func TestFuncFlagsEmpty(t *testing.T) {
	startCmdFlags.chefServer = false
	startCmdFlags.automate = false
	startCmdFlags.opensearch = false
	startCmdFlags.postgresql = false
	infra := getMockInfra()
	err := runStartCommandHA(infra, []string{})
	if err != nil {
		assert.EqualError(t, err, "\nNot able to start one or more nodes in postgresql: \nopen : no such file or directory")
	}
	assert.Nil(t, err)
}

func createMockSSHUtilMap(ips []string, connectErr error, execOutput string, execErr error) map[string]SSHUtil {
	sshUtilMap := make(map[string]SSHUtil)
	for _, ip := range ips {
		sshUtilMap[ip] = getMockSSHUtil(&SSHConfig{}, connectErr, execOutput, execErr)
	}
	return sshUtilMap
}
