// Copyright © 2017 Chef Software

package main

import (
	"container/list"
	"fmt"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
)

const configShow = `[global]
[global.v1]
  fqdn = "A2-6fdcfc05-automate-lb-355276303.sa-east-1.elb.amazonaws.com"
  [global.v1.log]
	redirect_sys_log = true
	redirect_log_file_path = "/var/tmp/"
	compress_rotated_logs = true
	max_size_rotate_logs = "10M"
	max_number_rotated_logs = 2
  [global.v1.external]
	[global.v1.external.postgresql]
	  enable = true
	  nodes = ["10.0.0.80:7432", "10.0.0.140:7432", "10.0.0.222:7432"]
	  [global.v1.external.postgresql.backup]
		enable = true
	  [global.v1.external.postgresql.auth]
		scheme = "password"
		[global.v1.external.postgresql.auth.password]
		  [global.v1.external.postgresql.auth.password.superuser]
			username = "admin"
			password = "admin"
		  [global.v1.external.postgresql.auth.password.dbuser]
			username = "admin"
			password = "admin"`

func Test_getVersionBasedOnFlag(t *testing.T) {
	t.Run("Version only for bastion", func(t *testing.T) {
		VersionCommandFlags.isBastion = true
		err := getVersionBasedOnFlag()
		assert.Nil(t, err)
	})
	t.Run("Version for bastion along with service flag", func(t *testing.T) {
		VersionCommandFlags.isBastion = true
		VersionCommandFlags.isAutomate = true
		err := getVersionBasedOnFlag()
		assert.Nil(t, err)
	})
	t.Run("Version for all nodes including bastion", func(t *testing.T) {
		VersionCommandFlags.isBastion = false
		VersionCommandFlags.isAutomate = false
		err := getVersionBasedOnFlag()
		assert.Nil(t, err)
	})
}

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
	t.Run("Error", func(t *testing.T) {
		mockCmdExecutor := &MockRemoteCmdExecutor{
			ExecuteFunc: func() (map[string][]*CmdResult, error) {
				return nil, nil
			},
			ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
				//return dummy result
				return map[string][]*CmdResult{}, errors.Errorf("Error sample")
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
		assert.Error(t, err)
		assert.Empty(t, versionMap)
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
		csIps := []string{TEST_IP_1}
		infra := &AutomateHAInfraDetails{} // Replace with appropriate initialization

		versionMap, err := getInfraServerVersion(csIps, infra, mockCmdExecutor)
		assert.NoError(t, err)
		assert.NotEmpty(t, versionMap)
		assert.Contains(t, versionMap, TEST_IP_1)
		assert.Equal(t, "15.4.0", versionMap[TEST_IP_1])
	})
	t.Run("Success", func(t *testing.T) {
		mockCmdExecutor := &MockRemoteCmdExecutor{
			ExecuteFunc: func() (map[string][]*CmdResult, error) {
				return nil, nil
			},
			ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
				//return dummy result
				return map[string][]*CmdResult{}, errors.Errorf("Error sample")
			},
			GetSshUtilFunc: func() SSHUtil {
				return &MockSSHUtilsImpl{
					connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
						return "", nil
					},
				}
			},
		}
		csIps := []string{TEST_IP_1}
		infra := &AutomateHAInfraDetails{} // Replace with appropriate initialization

		versionMap, err := getInfraServerVersion(csIps, infra, mockCmdExecutor)
		assert.Error(t, err)
		assert.Empty(t, versionMap)
	})
}

func Test_getOpensearchVersion(t *testing.T) {
	t.Run("Success Chef Managed", func(t *testing.T) {
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
							Output: `package                                               type        desired  state  elapsed (s)  pid   group
							chef/automate-ha-opensearch/1.3.7/20230223065900      standalone  up       up     975546       2872  automate-ha-opensearch.default
							chef/automate-ha-elasticsidecar/0.1.0/20230223070538  standalone  up       up     975546       2949  automate-ha-elasticsidecar.default`,
							Error: nil,
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
		osIps := []string{TEST_IP_1}
		infra := &AutomateHAInfraDetails{} // Replace with appropriate initialization

		versionMap, err := getOpensearchVersion(osIps, infra, false, mockCmdExecutor)
		assert.NoError(t, err)
		assert.NotEmpty(t, versionMap)
		assert.Contains(t, versionMap, TEST_IP_1)
		assert.Equal(t, "1.3.7", versionMap[TEST_IP_1])
	})
	t.Run("Error Chef Managed", func(t *testing.T) {
		mockCmdExecutor := &MockRemoteCmdExecutor{
			ExecuteFunc: func() (map[string][]*CmdResult, error) {
				return nil, nil
			},
			ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
				//return dummy result
				return map[string][]*CmdResult{}, errors.Errorf("Error sample")
			},
			GetSshUtilFunc: func() SSHUtil {
				return &MockSSHUtilsImpl{
					connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
						return "", nil
					},
				}
			},
		}
		osIps := []string{TEST_IP_1}
		infra := &AutomateHAInfraDetails{} // Replace with appropriate initialization

		versionMap, err := getOpensearchVersion(osIps, infra, false, mockCmdExecutor)
		assert.Error(t, err)
		assert.Empty(t, versionMap)
	})
	t.Run("Success Self Managed", func(t *testing.T) {
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
							Output:      `% Total % Received % Xferd Average Speed Time Time Time Current Dload Upload Total Spent Left Speed 100 565 100 565 0 0 551k 0 --:--:-- --:--:-- --:--:-- 551k { "name" : "ip-10-0-0-76", "cluster_name" : "opensearch", "cluster_uuid" : "3t-WAhYoTVa5F6-vcGbRTw", "version" : { "distribution" : "opensearch", "number" : "1.3.7", "build_type" : "tar", "build_hash" : "db18a0d5a08b669fb900c00d81462e221f4438ee", "build_date" : "2022-12-07T22:59:20.186520Z", "build_snapshot" : false, "lucene_version" : "8.10.1", "minimum_wire_compatibility_version" : "6.8.0", "minimum_index_compatibility_version" : "6.0.0-beta1" }, "tagline" : "The OpenSearch Project: https://opensearch.org/" }`,
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
		osIps := []string{TEST_IP_1}
		infra := &AutomateHAInfraDetails{} // Replace with appropriate initialization
		infra.Outputs.AutomatePrivateIps.Value = []string{ValidIP, ValidIP1}
		versionMap, err := getOpensearchVersion(osIps, infra, true, mockCmdExecutor)
		assert.NoError(t, err)
		assert.NotEmpty(t, versionMap)
		assert.Contains(t, versionMap, TEST_IP_1)
		assert.Equal(t, "1.3.7", versionMap[TEST_IP_1])
	})
	t.Run("Error Self Managed", func(t *testing.T) {
		mockCmdExecutor := &MockRemoteCmdExecutor{
			ExecuteFunc: func() (map[string][]*CmdResult, error) {
				return nil, nil
			},
			ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
				//return dummy result
				return map[string][]*CmdResult{}, errors.Errorf("Error sample")
			},
			GetSshUtilFunc: func() SSHUtil {
				return &MockSSHUtilsImpl{
					connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
						return "", nil
					},
				}
			},
		}
		osIps := []string{TEST_IP_1}
		infra := &AutomateHAInfraDetails{} // Replace with appropriate initialization
		infra.Outputs.AutomatePrivateIps.Value = []string{ValidIP, ValidIP1}
		versionMap, err := getOpensearchVersion(osIps, infra, true, mockCmdExecutor)
		assert.Error(t, err)
		assert.Empty(t, versionMap)
	})
}

func Test_getPostgresqlVersion(t *testing.T) {

	t.Run("Success Chef Managed", func(t *testing.T) {
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
							Output: `package                                            type        desired  state  elapsed (s)  pid   group
							chef/automate-ha-postgresql/13.5.0/20230130151541  standalone  up       up     975746       3269  automate-ha-postgresql.default
							chef/automate-ha-haproxy/2.2.14/20230130151541     standalone  up       up     975752       3137  automate-ha-haproxy.default
							chef/automate-ha-pgleaderchk/0.1.0/20230130152444  standalone  up       up     975751       3146  automate-ha-pgleaderchk.default`,
							Error: nil,
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
		pgIps := []string{TEST_IP_1}
		infra := &AutomateHAInfraDetails{} // Replace with appropriate initialization

		versionMap, err := getPostgresqlVersion(pgIps, infra, false, mockCmdExecutor)
		assert.NoError(t, err)
		assert.NotEmpty(t, versionMap)
		assert.Contains(t, versionMap, TEST_IP_1)
		assert.Equal(t, "13.5.0", versionMap[TEST_IP_1])
	})
	t.Run("Error Chef Managed", func(t *testing.T) {
		mockCmdExecutor := &MockRemoteCmdExecutor{
			ExecuteFunc: func() (map[string][]*CmdResult, error) {
				return nil, nil
			},
			ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
				//return dummy result
				return map[string][]*CmdResult{}, errors.Errorf("Error sample")

			},
			GetSshUtilFunc: func() SSHUtil {
				return &MockSSHUtilsImpl{
					connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
						return "", nil
					},
				}
			},
		}
		pgIps := []string{TEST_IP_1}
		infra := &AutomateHAInfraDetails{} // Replace with appropriate initialization

		versionMap, err := getPostgresqlVersion(pgIps, infra, false, mockCmdExecutor)
		assert.Error(t, err)
		assert.Empty(t, versionMap)
	})
	t.Run("Error Self Managed", func(t *testing.T) {
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
							Output:      `PostgreSQL 13.5 on x86_64-pc-linux-gnu, compiled by gcc.real (GCC) 9.4.0, 64-bit`,
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
		pgIps := []string{TEST_IP_1}
		infra := &AutomateHAInfraDetails{} // Replace with appropriate initialization

		infra.Outputs.AutomatePrivateIps.Value = []string{ValidIP, ValidIP1}

		versionMap, err := getPostgresqlVersion(pgIps, infra, true, mockCmdExecutor)
		assert.Error(t, err)
		assert.Equal(t, "Couldn't get Super user password and name from config", err.Error())
		assert.Empty(t, versionMap)
	})

}

func Test_splitIP(t *testing.T) {
	t.Run("Single IP", func(t *testing.T) {
		input := "192.168.0.1"
		expectedOutput := []string{"192.168.0.1"}

		output := splitIP(input)

		assert.Equal(t, expectedOutput, output)
	})

	t.Run("Multiple IPs", func(t *testing.T) {
		input := "192.168.0.1, 192.168.0.2, 192.168.0.3"
		expectedOutput := []string{"192.168.0.1", "192.168.0.2", "192.168.0.3"}

		output := splitIP(input)

		assert.Equal(t, expectedOutput, output)
	})

	t.Run("Empty Input", func(t *testing.T) {
		input := ""
		expectedOutput := []string{""}

		output := splitIP(input)

		assert.Equal(t, expectedOutput, output)
	})
}

func Test_getIPAddressesFromFlagOrInfra(t *testing.T) {
	infra := &AutomateHAInfraDetails{}
	infra.Outputs.AutomatePrivateIps.Value = []string{ValidIP, ValidIP1}
	infra.Outputs.ChefServerPrivateIps.Value = []string{ValidIP2, ValidIP3}
	infra.Outputs.OpensearchPrivateIps.Value = []string{ValidIP4, ValidIP5, ValidIP6}
	infra.Outputs.PostgresqlPrivateIps.Value = []string{ValidIP7, ValidIP8, ValidIP9}

	automateIps, chefServerIps, opensearchIps, postgresqlIps, errList := getIPAddressesFromFlagOrInfra(infra)
	assert.Equal(t, errList.Len(), 0)
	assert.Equal(t, automateIps, []string{ValidIP, ValidIP1})
	assert.Equal(t, chefServerIps, []string{ValidIP2, ValidIP3})
	assert.Equal(t, opensearchIps, []string{ValidIP4, ValidIP5, ValidIP6})
	assert.Equal(t, postgresqlIps, []string{ValidIP7, ValidIP8, ValidIP9})
}

func Test_getIPAddressesFromFlag(t *testing.T) {
	t.Run("Valid config", func(t *testing.T) {
		infra := &AutomateHAInfraDetails{}
		infra.Outputs.AutomatePrivateIps.Value = []string{ValidIP, ValidIP1}
		infra.Outputs.OpensearchPrivateIps.Value = []string{ValidIP4, ValidIP5, ValidIP6}
		infra.Outputs.PostgresqlPrivateIps.Value = []string{ValidIP7, ValidIP8, ValidIP9}

		VersionCommandFlags.isAutomate = true
		VersionCommandFlags.isOpenSearch = true
		VersionCommandFlags.isPostgresql = true
		VersionCommandFlags.node = fmt.Sprintf("%s,%s,%s,%s", ValidIP, ValidIP3, ValidIP5, ValidIP8)

		automateIps, chefServerIps, opensearchIps, postgresqlIps, _ := getIPAddressesFromFlagOrInfra(infra)
		assert.Equal(t, automateIps, []string{ValidIP})
		assert.Equal(t, opensearchIps, []string{ValidIP5})
		assert.Equal(t, postgresqlIps, []string{ValidIP8})
		assert.Empty(t, chefServerIps)
	})
	t.Run("Invalid config", func(t *testing.T) {
		infra := &AutomateHAInfraDetails{}
		infra.Outputs.AutomatePrivateIps.Value = []string{ValidIP, ValidIP1}
		infra.Outputs.OpensearchPrivateIps.Value = []string{ValidIP4, ValidIP5, ValidIP6}
		infra.Outputs.PostgresqlPrivateIps.Value = []string{ValidIP7, ValidIP8, ValidIP9}

		VersionCommandFlags.isAutomate = true
		VersionCommandFlags.node = fmt.Sprintf("%s,%s", TEST_IP, TEST_IP2)

		automateIps, chefServerIps, opensearchIps, postgresqlIps, errorList := getIPAddressesFromFlagOrInfra(infra)
		assert.Empty(t, automateIps)
		assert.Empty(t, opensearchIps)
		assert.Empty(t, postgresqlIps)
		assert.Empty(t, chefServerIps)
		assert.NotEmpty(t, errorList)
	})

}

func Test_getPgAuth(t *testing.T) {
	t.Run("Valid config", func(t *testing.T) {
		sshUtil := GetMockSSHUtil(&SSHConfig{}, nil, configShow, nil, "", nil)

		su, sp := getPgAuth(sshUtil)

		assert.NotEmpty(t, su)
		assert.NotEmpty(t, sp)
		assert.Equal(t, "admin", su)
		assert.Equal(t, "admin", sp)
	})
	t.Run("InValid config", func(t *testing.T) {
		sshUtil := GetMockSSHUtil(&SSHConfig{}, nil, "Invalid", nil, "", nil)

		su, sp := getPgAuth(sshUtil)

		assert.Empty(t, su)
		assert.Empty(t, sp)
	})
	t.Run("Error config", func(t *testing.T) {
		sshUtil := GetMockSSHUtil(&SSHConfig{}, nil, "", errors.Errorf("remote execution"), "", nil)

		su, sp := getPgAuth(sshUtil)

		assert.Empty(t, su)
		assert.Empty(t, sp)
	})

}

func Test_validateIPAddresses(t *testing.T) {
	t.Run("Valid IPs", func(t *testing.T) {
		expectedIPFound := []string{"198.51.100.0", "198.51.100.1"}
		expectedIPNotFound := []string{"198.51.100.2"}
		expectedErrorList := list.New()

		mockInfra := &AutomateHAInfraDetails{}
		mockInfra.Outputs.AutomatePrivateIps.Value = []string{ValidIP, ValidIP1}
		mockInfra.Outputs.ChefServerPrivateIps.Value = []string{ValidIP2, ValidIP3}
		mockInfra.Outputs.OpensearchPrivateIps.Value = []string{ValidIP4, ValidIP5, ValidIP6}
		mockInfra.Outputs.PostgresqlPrivateIps.Value = []string{ValidIP7, ValidIP8, ValidIP9}
		errorList := list.New()

		ipFound, ipNotFound, errorList := validateIPAddresses(errorList, []string{"198.51.100.0", "198.51.100.1", "198.51.100.2"}, "automate", "error message", mockInfra)

		assert.ElementsMatch(t, expectedIPFound, ipFound)
		assert.ElementsMatch(t, expectedIPNotFound, ipNotFound)
		assert.Equal(t, expectedErrorList, errorList)
	})

	t.Run("Invalid IPs", func(t *testing.T) {
		expectedErrorList := list.New()
		expectedErrorList.PushBack("Incorrect automate IP, 192.168.0 error message")

		mockInfra := &AutomateHAInfraDetails{}
		mockInfra.Outputs.AutomatePrivateIps.Value = []string{ValidIP, ValidIP1}
		mockInfra.Outputs.ChefServerPrivateIps.Value = []string{ValidIP2, ValidIP3}
		mockInfra.Outputs.OpensearchPrivateIps.Value = []string{ValidIP4, ValidIP5, ValidIP6}
		mockInfra.Outputs.PostgresqlPrivateIps.Value = []string{ValidIP7, ValidIP8, ValidIP9}
		errorList := list.New()

		ipFound, ipNotFound, errorList := validateIPAddresses(errorList, []string{"192.168.0", "192.168.0.6"}, "automate", " error message", mockInfra)

		assert.Empty(t, ipFound)
		assert.ElementsMatch(t, []string{"192.168.0", "192.168.0.6"}, ipNotFound)
		assert.Equal(t, expectedErrorList, errorList)
	})
}
