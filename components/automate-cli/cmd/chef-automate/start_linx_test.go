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
		sshUtil       SSHUtil
		frontendIps   []string
		remoteService string
		isError       bool
		err           error
	}{
		{
			[]string{"some_args"},
			getMockSSHUtil(&SSHConfig{}, nil, "", nil),
			[]string{"127.0.1.3", "127.0.1.4", "127.0.1.5"},
			"automate",
			false,
			nil,
		},
		{
			[]string{"some_args"},
			getMockSSHUtil(&SSHConfig{}, nil, "", nil),
			argsEmpty,
			"chef-server",
			true,
			status.Errorf(1, "No chef-server IPs are found"),
		},
		{
			[]string{"some_args"},
			NewSSHUtil(&SSHConfig{}),
			argsEmpty,
			"automate",
			true,
			errors.Errorf("No automate IPs are found"),
		},
		{
			[]string{"some_args"},
			getMockSSHUtil(&SSHConfig{}, nil, "", errors.New("Process exited with status 1")),
			[]string{"127.0.0.3"},
			"chef-server",
			true,
			errors.New("Not able to start one or more nodes in chef-server: \nProcess exited with status 1"),
		},
	}

	for _, testCase := range testCases {
		err := checkNodes(testCase.args, testCase.sshUtil, testCase.frontendIps, testCase.remoteService, getMockWriterImpl())
		if testCase.isError {
			assert.EqualError(t, testCase.err, err.Error())
		}
	}
}

func TestStartForBackEndNodes(t *testing.T) {
	testCases := []struct {
		args          []string
		sshUtil       SSHUtil
		frontendIps   []string
		remoteService string
		isError       bool
		err           error
	}{
		{
			[]string{"some_args"},
			getMockSSHUtil(&SSHConfig{}, nil, "", nil),
			[]string{"127.0.0.3", "127.0.0.4", "127.0.0.5"},
			"opensearch",
			false,
			nil,
		},
		{
			[]string{"some_args"},
			getMockSSHUtil(&SSHConfig{}, nil, "", nil),
			[]string{},
			"postgresql",
			true,
			status.Errorf(1, "No postgresql IPs are found"),
		},
		{
			[]string{"some_args"},
			getMockSSHUtil(&SSHConfig{}, nil, "", errors.New("Process exited with status 1")),
			[]string{"127.0.0.3"},
			"postgresql",
			true,
			errors.New("Not able to start one or more nodes in postgresql: \nProcess exited with status 1\nProcess exited with status 1"),
		},
	}

	for _, testCase := range testCases {
		err := checkNodes(testCase.args, testCase.sshUtil, testCase.frontendIps, testCase.remoteService, getMockWriterImpl())
		if testCase.isError {
			assert.EqualError(t, testCase.err, err.Error())
		}
	}
}

func TestForRunCommand(t *testing.T) {
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
			"sudo chef-automate start",
			"Error",
			true,
			errors.New("Error"),
		},
		{
			[]string{"some_args"},
			getMockSSHUtil(&SSHConfig{}, nil, "", errors.New("error")),
			"sudo chef-automate start",
			"",
			true,
			errors.New("error"),
		},
		{
			[]string{"some_args"},
			getMockSSHUtil(&SSHConfig{}, nil, "Starting Chef-automate", nil),
			"sudo chef-automate start",
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

func getMockInfra() *AutomteHAInfraDetails {
	infra := &AutomteHAInfraDetails{}
	infra.Outputs.AutomatePrivateIps.Value = []string{"127.0.0.0"}
	infra.Outputs.ChefServerPrivateIps.Value = []string{"127.0.0.1"}
	infra.Outputs.OpensearchPrivateIps.Value = []string{"127.0.0.2"}
	infra.Outputs.PostgresqlPrivateIps.Value = []string{"127..0.0.3"}
	return infra
}

func TestFunc(t *testing.T) {
	startCmdFlags.chef_server = true
	startCmdFlags.automate = false
	startCmdFlags.opensearch = false
	startCmdFlags.postgresql = false
	infra := getMockInfra()
	err := runStartCommandHA(infra, []string{"--cs"})
	assert.EqualError(t, err, "\nNot able to start one or more nodes in chef-server: \nopen : no such file or directory")
}

func TestFuncAutomate(t *testing.T) {
	startCmdFlags.chef_server = false
	startCmdFlags.automate = true
	startCmdFlags.opensearch = false
	startCmdFlags.postgresql = false
	infra := getMockInfra()
	err := runStartCommandHA(infra, []string{"--a2"})
	assert.EqualError(t, err, "\nNot able to start one or more nodes in automate: \nopen : no such file or directory")
}

func TestFuncOs(t *testing.T) {
	startCmdFlags.chef_server = false
	startCmdFlags.automate = false
	startCmdFlags.opensearch = true
	startCmdFlags.postgresql = false
	infra := getMockInfra()
	err := runStartCommandHA(infra, []string{"--os"})
	assert.EqualError(t, err, "\nNot able to start one or more nodes in opensearch: \nopen : no such file or directory\nopen : no such file or directory")
}
func TestFuncPg(t *testing.T) {
	startCmdFlags.chef_server = false
	startCmdFlags.automate = false
	startCmdFlags.opensearch = false
	startCmdFlags.postgresql = true
	infra := getMockInfra()
	err := runStartCommandHA(infra, []string{"--pg"})
	assert.EqualError(t, err, "\nNot able to start one or more nodes in postgresql: \nopen : no such file or directory\nopen : no such file or directory")
}
