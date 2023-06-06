package main

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/stretchr/testify/assert"
)

func TestBuildFrontEndStatusCmd(t *testing.T) {
	type testCase struct {
		flags           *statusCmdFlags
		expectedCommand string
	}

	testCases := []testCase{
		{
			flags: &statusCmdFlags{
				waitRefreshInterval: 2,
				waitTimeout:         600,
			},
			expectedCommand: "sudo chef-automate status -r 2 -t 600",
		},
		{
			flags: &statusCmdFlags{
				waitRefreshInterval: 2,
				waitTimeout:         600,
				waitForHealthy:      true,
			},
			expectedCommand: "sudo chef-automate status -w -r 2 -t 600",
		},
	}

	for _, testCase := range testCases {
		t.Run("Checking command creation", func(t *testing.T) {
			commandGet := buildFrontEndStatusCmd(testCase.flags)
			assert.Equal(t, testCase.expectedCommand, commandGet)
		})
	}
}

func TestHandleManagedServiceError(t *testing.T) {

	testCases := []struct {
		flags          *statusCmdFlags
		errorExepected error
	}{
		{
			flags: &statusCmdFlags{
				postgresql: true,
			},
			errorExepected: status.Errorf(status.InvalidCommandArgsError, STATUS_ERROR_ON_SELF_MANAGED, POSTGRESQL),
		},
		{
			flags: &statusCmdFlags{
				opensearch: true,
			},
			errorExepected: status.Errorf(status.InvalidCommandArgsError, STATUS_ERROR_ON_SELF_MANAGED, OPENSEARCH),
		},
		{
			flags:          &statusCmdFlags{},
			errorExepected: status.Errorf(status.InvalidCommandArgsError, "Please provide supported flag"),
		},
		{
			flags: &statusCmdFlags{
				postgresql: true,
				opensearch: true,
			},
			errorExepected: nil,
		},
	}

	for _, testCase := range testCases {
		err := handleManagedServiceError(testCase.flags)

		if testCase.errorExepected != nil {
			assert.EqualError(t, err, testCase.errorExepected.Error())
		} else {
			assert.Nil(t, err)
		}
	}
}
