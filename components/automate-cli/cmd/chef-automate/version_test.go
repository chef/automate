// Copyright © 2017 Chef Software

package main

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/stretchr/testify/assert"
)

func Test_getChefAutomateVersion(t *testing.T) {
	t.Run("Success", func(t *testing.T) {
		mockCmdExecutor := &MockRemoteCmdExecutor{
			ExecuteFunc: func() (map[string][]*CmdResult, error) {
				return nil, nil
			},
			ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
				//return dummy result
				return map[string][]*CmdResult{
					TEST_IP_1: {
						{
							ScriptName:  "",
							HostIP:      TEST_IP_1,
							OutputFiles: []string{},
							Output:      "Version: 2\n CLI Build: 20230502070346\n Server Build: 4.5.177",
							Error:       nil,
						},
					},
				}, nil
			},
			GetSshUtilFunc: func() SSHUtil {
				return &MockSSHUtilsImpl{
					connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
						return "", nil
					},
				}
			},
		}
		automateIps := []string{TEST_IP_1}
		infra := &AutomateHAInfraDetails{} // Replace with appropriate initialization

		versionMap, err := getChefAutomateVersion(automateIps, infra, mockCmdExecutor)
		assert.NoError(t, err)
		assert.NotEmpty(t, versionMap)
		assert.Contains(t, versionMap, TEST_IP_1)
	})
}

func Test_extractVersion(t *testing.T) {
	t.Run("Returns OK", func(t *testing.T) {
		versionString := "PostgreSQL 13.5 on x86_64-pc-linux-gnu, compiled by gcc.real (GCC) 9.4.0, 64-bit"
		pattern := `PostgreSQL (\d+\.\d+)`

		versionNumber, err := extractVersion(versionString, pattern)
		assert.Equal(t, nil, err)
		assert.Equal(t, "13.5", versionNumber)
	})
	t.Run("Error1", func(t *testing.T) {
		versionString := "Invalid version string"
		pattern := `PostgreSQL (\d+\.\d+)`

		versionNumber, err := extractVersion(versionString, pattern)
		assert.NotNil(t, err)
		assert.Equal(t, "no version string found", err.Error())
		assert.Empty(t, versionNumber)
	})
	t.Run("Error2", func(t *testing.T) {
		versionString := "PostgreSQL 13.5 on x86_64-pc-linux-gnu, compiled by gcc.real (GCC) 9.4.0, 64-bit"
		pattern := `[`

		versionNumber, err := extractVersion(versionString, pattern)
		assert.NotNil(t, err)
		assert.Empty(t, versionNumber)
	})

}

func Test_filterPackage(t *testing.T) {

	t.Run("Found", func(t *testing.T) {
		serviceList := []models.ServiceDetails{
			{ServiceName: "mypackage.service1"},
			{ServiceName: "mypackageone.service2"},
			{ServiceName: "otherpackage.service3"},
		}

		packageName := "mypackage"

		result := filterPackage(&serviceList, packageName)

		assert.NotNil(t, result)
		assert.Equal(t, "mypackage.service1", result.ServiceName)

	})
	t.Run("Not found", func(t *testing.T) {
		serviceList := []models.ServiceDetails{
			{ServiceName: "mypackage.service1"},
			{ServiceName: "mypackageone.service2"},
			{ServiceName: "otherpackage.service3"},
		}

		packageName := "pkg"

		result := filterPackage(&serviceList, packageName)

		assert.Nil(t, result)

	})
}

func Test_getInfraServerVersion(t *testing.T) {
	t.Run("Success", func(t *testing.T) {
		mockCmdExecutor := &MockRemoteCmdExecutor{
			ExecuteFunc: func() (map[string][]*CmdResult, error) {
				return nil, nil
			},
			ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
				//return dummy result
				return map[string][]*CmdResult{
					TEST_IP_1: {
						{
							ScriptName:  "",
							HostIP:      TEST_IP_1,
							OutputFiles: []string{},
							Output:      "Chef Automate Chef server 15.4.0/20230223065651",
							Error:       nil,
						},
					},
				}, nil
			},
			GetSshUtilFunc: func() SSHUtil {
				return &MockSSHUtilsImpl{
					connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
						return "", nil
					},
				}
			},
		}
		automateIps := []string{TEST_IP_1}
		infra := &AutomateHAInfraDetails{} // Replace with appropriate initialization

		versionMap, err := getInfraServerVersion(automateIps, infra, mockCmdExecutor)
		assert.NoError(t, err)
		assert.NotEmpty(t, versionMap)
		assert.Contains(t, versionMap, TEST_IP_1)
		assert.Equal(t, "15.4.0", versionMap[TEST_IP_1])
	})
}
