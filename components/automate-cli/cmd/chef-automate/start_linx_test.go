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
		isError        bool
		err            error
	}{
		{
			[]string{"some_args"},
			getMockSSHUtil(&SSHConfig{}, nil, "Error", nil),
			"sudo chef-automate start",
			true,
			errors.New("Error"),
		},
		{
			[]string{"some_args"},
			getMockSSHUtil(&SSHConfig{}, nil, "", errors.New("error")),
			"sudo chef-automate start",
			true,
			errors.New("error"),
		},
	}
	for _, testCase := range testCases {
		_, err := runCommand(testCase.scriptCommands, testCase.sshUtil)
		if testCase.isError {
			assert.EqualError(t, testCase.err, err.Error())
		}
	}
}

func TestErrorOnManaged(t *testing.T) {
	testCases := []struct {
		isPostgresql bool
		isOpenSearch bool
		errorWant    error
	}{
		{
			true,
			false,
			errors.Errorf("Start services in %s for externally configured is not supported", "Postgresql"),
		},
		{
			false,
			true,
			errors.Errorf("Start services in %s for externally configured is not supported", "OpenSearch"),
		},
		{
			false,
			false,
			nil,
		},
	}

	for _, testCase := range testCases {
		errGot := errorOnManaged(testCase.isPostgresql, testCase.isOpenSearch)
		if errGot == nil {
			assert.Equal(t, testCase.errorWant, errGot)
		} else {
			assert.EqualError(t, testCase.errorWant, errGot.Error())
		}
	}
}
