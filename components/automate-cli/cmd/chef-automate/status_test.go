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
			errorExepected: nil,
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

func TestConstructNodeMapForStatus(t *testing.T) {
	type testCase struct {
		flags           *statusCmdFlags
		nodeMapExpected *NodeTypeAndCmd
	}
	infra := &AutomateHAInfraDetails{}

	testCases := []testCase{
		{
			flags: &statusCmdFlags{
				waitForHealthy:      false,
				waitTimeout:         600,
				waitRefreshInterval: 10,
				automate:            true,
			},
			nodeMapExpected: &NodeTypeAndCmd{
				Frontend: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:                      "sudo chef-automate status -r 10 -t 600",
						WaitTimeout:              600,
						SkipPrintOutput:          true,
						HideSSHConnectionMessage: true,
					},
				},
				Automate: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:                      "sudo chef-automate status -r 10 -t 600",
						WaitTimeout:              600,
						ErrorCheckEnableInOutput: true,
						NodeIps:                  []string{""},
						NodeType:                 true,
						SkipPrintOutput:          true,
						HideSSHConnectionMessage: true,
					},
				},
				ChefServer: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:                      "sudo chef-automate status -r 10 -t 600",
						WaitTimeout:              600,
						ErrorCheckEnableInOutput: true,
						NodeIps:                  []string{""},
						NodeType:                 false,
						SkipPrintOutput:          true,
						HideSSHConnectionMessage: true,
					},
				},
				Postgresql: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:                      BACKEND_STATUS,
						NodeIps:                  []string{""},
						ErrorCheckEnableInOutput: true,
						NodeType:                 false,
						WaitTimeout:              600,
						SkipPrintOutput:          true,
						HideSSHConnectionMessage: true,
					},
				},
				Opensearch: &Cmd{
					CmdInputs: &CmdInputs{
						Cmd:                      BACKEND_STATUS,
						NodeIps:                  []string{""},
						ErrorCheckEnableInOutput: true,
						NodeType:                 false,
						WaitTimeout:              600,
						SkipPrintOutput:          true,
						HideSSHConnectionMessage: true,
					},
				},
				Infra: infra,
			},
		},
	}

	for _, testCase := range testCases {
		nodeMapGet := constructNodeMapForStatus(testCase.flags, infra)
		assert.Equal(t, testCase.nodeMapExpected, nodeMapGet)
	}
}
