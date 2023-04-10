package main

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/status"
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
	}

	for _, testCase := range testCases {
		err := checkNodes(testCase.args, testCase.sshUtil, testCase.frontendIps, testCase.remoteService, getMockWriterImpl())
		if testCase.isError {
			assert.EqualError(t, testCase.err, err.Error())
		}
	}
}
