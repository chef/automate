package main

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestStartForFrontEndNodes(t *testing.T) {
	testCases := []struct {
		args          []string
		sshUtil       SSHUtil
		frontendIps   []string
		remoteService string
		timestamp     string
		isError       bool
		err           error
	}{
		{
			[]string{"some_args"},
			getMockSSHUtil(&SSHConfig{}, nil, "Start Command is completed", nil),
			[]string{"127.0.0.3", "127.0.0.4", "127.0.0.5"},
			"automate",
			"20060102150405",
			false,
			nil,
		},
	}

	for _, testCase := range testCases {
		err := startFrontEndNodes(testCase.args, testCase.sshUtil, testCase.frontendIps, testCase.remoteService, testCase.timestamp, getMockWriterImpl())
		if testCase.isError {
			fmt.Println(err)
			assert.Error(t, err)
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
		timestamp     string
		isError       bool
		err           error
	}{
		{
			[]string{"some_args"},
			getMockSSHUtil(&SSHConfig{}, nil, "Start Command is completed", nil),
			[]string{"127.0.0.3", "127.0.0.4", "127.0.0.5"},
			"opensearch",
			"20060102150405",
			false,
			nil,
		},
	}

	for _, testCase := range testCases {
		err := startBackEndNodes(testCase.args, testCase.sshUtil, testCase.frontendIps[0], testCase.remoteService, testCase.timestamp, getMockWriterImpl())
		if testCase.isError {
			fmt.Println(err)
			assert.Error(t, err)
			assert.EqualError(t, testCase.err, err.Error())
		}
	}
}
