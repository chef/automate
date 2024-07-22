package main

import (
	"errors"
	"fmt"
	"strings"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
)

const (
	invalidIpAddress = `
Incorrect automate IP, 127.0.0 IP address validation failed
Incorrect automate IP, 127.0.1 IP address validation failed
Incorrect automate IP, 127.0.2 IP address validation failed
Incorrect automate IP, 127.0.3 IP address validation failed
Incorrect chef-server IP, 127.0.0 IP address validation failed
Incorrect chef-server IP, 127.0.1 IP address validation failed
Incorrect chef-server IP, 127.0.2 IP address validation failed
Incorrect chef-server IP, 127.0.3 IP address validation failed
Incorrect opensearch IP, 127.0.0 IP address validation failed
Incorrect opensearch IP, 127.0.1 IP address validation failed
Incorrect opensearch IP, 127.0.2 IP address validation failed
Incorrect opensearch IP, 127.0.3 IP address validation failed
Incorrect postgresql IP, 127.0.0 IP address validation failed
Incorrect postgresql IP, 127.0.1 IP address validation failed
Incorrect postgresql IP, 127.0.2 IP address validation failed
Incorrect postgresql IP, 127.0.3 IP address validation failed`
	displayBE = `+------------+--------------+--------+--------------+-----+-------------+---------+
| NAME       | IP ADDRESS   | HEALTH | PROCESS      | LAG | UPTIME      | ROLE    |
+------------+--------------+--------+--------------+-----+-------------+---------+
| postgresql | 198.51.100.7 | ERROR  | down (pid: ) | NA  | 0d 0h 0m 0s | Unknown |
+------------+--------------+--------+--------------+-----+-------------+---------+`
	pgFollowerNode = `+------------+--------------+--------+----------------+-----------------------+---------------+----------+
| NAME       | IP ADDRESS   | HEALTH | PROCESS        | LAG                   | UPTIME        | ROLE     |
+------------+--------------+--------+----------------+-----------------------+---------------+----------+
| postgresql | 198.51.100.7 | OK     | up (pid: 2760) | 0 bytes and 1 seconds |`
	displayFEAutomate         = `| automate    | 198.51.100.1 | OK     | "green" (Active: 100.0) |`
	displayFECS               = `| chef-server | 198.51.100.2 | OK     | "green" (Active: 100.0) |`
	mockA2haHabitatAutoTfvars = "../../pkg/testfiles/a2ha_habitat.auto.tfvars"
)

func TestCheckIPAddressesFromInfra(t *testing.T) {
	infra := &AutomateHAInfraDetails{}
	infra.Outputs.AutomatePrivateIps.Value = []string{ValidIP, ValidIP1}
	infra.Outputs.ChefServerPrivateIps.Value = []string{ValidIP2, ValidIP3}
	infra.Outputs.OpensearchPrivateIps.Value = []string{ValidIP4, ValidIP5, ValidIP6}
	infra.Outputs.PostgresqlPrivateIps.Value = []string{ValidIP7, ValidIP8, ValidIP9}
	ss := NewStatusSummary(infra, FeStatus{}, BeStatus{}, 10, time.Second, &StatusSummaryCmdFlags{}, &MockRemoteCmdExecutor{
		ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
			return nil, nil
		},
	})

	automateIps, chefServerIps, opensearchIps, postgresqlIps, errList := ss.(*Summary).getIPAddressesFromFlagOrInfra()
	assert.Equal(t, errList.Len(), 0)
	assert.Equal(t, automateIps, []string{ValidIP, ValidIP1})
	assert.Equal(t, chefServerIps, []string{ValidIP2, ValidIP3})
	assert.Equal(t, opensearchIps, []string{ValidIP4, ValidIP5, ValidIP6})
	assert.Equal(t, postgresqlIps, []string{ValidIP7, ValidIP8, ValidIP9})
}

func TestCheckIPAddressesByServicesAndIpFromFlag(t *testing.T) {
	infra := &AutomateHAInfraDetails{}
	infra.Outputs.AutomatePrivateIps.Value = []string{ValidIP, ValidIP1}
	infra.Outputs.ChefServerPrivateIps.Value = []string{ValidIP2, ValidIP3}
	infra.Outputs.OpensearchPrivateIps.Value = []string{ValidIP4, ValidIP5, ValidIP6}
	infra.Outputs.PostgresqlPrivateIps.Value = []string{ValidIP7, ValidIP8, ValidIP9}

	ss := NewStatusSummary(infra, FeStatus{}, BeStatus{}, 10, time.Second, &StatusSummaryCmdFlags{
		node:         fmt.Sprintf("%s,%s,%s,%s", ValidIP, ValidIP3, ValidIP5, ValidIP8),
		isAutomate:   true,
		isChefServer: true,
		isOpenSearch: true,
		isPostgresql: true,
	}, &MockRemoteCmdExecutor{
		ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
			return nil, nil
		},
	})

	automateIps, chefServerIps, opensearchIps, postgresqlIps, errList := ss.(*Summary).getIPAddressesFromFlagOrInfra()
	assert.Equal(t, errList.Len(), 0)
	assert.Equal(t, automateIps, []string{ValidIP})
	assert.Equal(t, chefServerIps, []string{ValidIP3})
	assert.Equal(t, opensearchIps, []string{ValidIP5})
	assert.Equal(t, postgresqlIps, []string{ValidIP8})
}

func TestCheckIPAddressesOnlyByServices(t *testing.T) {
	infra := &AutomateHAInfraDetails{}
	infra.Outputs.AutomatePrivateIps.Value = []string{ValidIP, ValidIP1}
	infra.Outputs.ChefServerPrivateIps.Value = []string{ValidIP2, ValidIP3}
	infra.Outputs.OpensearchPrivateIps.Value = []string{ValidIP4, ValidIP5, ValidIP6}
	infra.Outputs.PostgresqlPrivateIps.Value = []string{ValidIP7, ValidIP8, ValidIP9}

	ss := NewStatusSummary(infra, FeStatus{}, BeStatus{}, 10, time.Second, &StatusSummaryCmdFlags{
		isAutomate:   true,
		isChefServer: true,
		isOpenSearch: true,
		isPostgresql: true,
	}, &MockRemoteCmdExecutor{
		ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
			return nil, nil
		},
	})

	automateIps, chefServerIps, opensearchIps, postgresqlIps, errList := ss.(*Summary).getIPAddressesFromFlagOrInfra()
	assert.Equal(t, errList.Len(), 0)
	assert.Equal(t, automateIps, []string{ValidIP, ValidIP1})
	assert.Equal(t, chefServerIps, []string{ValidIP2, ValidIP3})
	assert.Equal(t, opensearchIps, []string{ValidIP4, ValidIP5, ValidIP6})
	assert.Equal(t, postgresqlIps, []string{ValidIP7, ValidIP8, ValidIP9})
}

func TestCheckIPAddressesError(t *testing.T) {
	infra := &AutomateHAInfraDetails{}
	infra.Outputs.AutomatePrivateIps.Value = []string{ValidIP, ValidIP1}
	infra.Outputs.ChefServerPrivateIps.Value = []string{ValidIP2, ValidIP3}
	infra.Outputs.OpensearchPrivateIps.Value = []string{ValidIP4, ValidIP5, ValidIP6}
	infra.Outputs.PostgresqlPrivateIps.Value = []string{ValidIP7, ValidIP8, ValidIP9}

	ss := NewStatusSummary(infra, FeStatus{}, BeStatus{}, 10, time.Second, &StatusSummaryCmdFlags{
		node:         "127.0.0.1,127.0.0.2",
		isAutomate:   true,
		isChefServer: true,
		isOpenSearch: true,
		isPostgresql: true,
	}, &MockRemoteCmdExecutor{
		ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
			return nil, nil
		},
	})

	_, _, _, _, errList := ss.(*Summary).getIPAddressesFromFlagOrInfra()
	assert.Equal(t, errList.Len(), 1)
	assert.Contains(t, getSingleErrorFromList(errList).Error(), "List of  ip address not found [127.0.0.1 127.0.0.2] does not match any node for Automate, Chef server, Opensearch, PostgreSQL service")
}

func TestCheckIPAddressesValidation(t *testing.T) {
	infra := &AutomateHAInfraDetails{}
	infra.Outputs.AutomatePrivateIps.Value = []string{ValidIP, ValidIP1}
	infra.Outputs.ChefServerPrivateIps.Value = []string{ValidIP2, ValidIP3}
	infra.Outputs.OpensearchPrivateIps.Value = []string{ValidIP4, ValidIP5, ValidIP6}
	infra.Outputs.PostgresqlPrivateIps.Value = []string{ValidIP7, ValidIP8, ValidIP9}

	ss := NewStatusSummary(infra, FeStatus{}, BeStatus{}, 10, time.Second, &StatusSummaryCmdFlags{
		node:         "127.0.0,127.0.1,127.0.2,127.0.3",
		isAutomate:   true,
		isChefServer: true,
		isOpenSearch: true,
		isPostgresql: true,
	}, &MockRemoteCmdExecutor{
		ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
			return nil, nil
		},
	})

	automateIps, chefServerIps, opensearchIps, postgresqlIps, errList := ss.(*Summary).getIPAddressesFromFlagOrInfra()
	assert.Equal(t, errList.Len(), 16)
	assert.Equal(t, automateIps, []string(nil))
	assert.Equal(t, chefServerIps, []string(nil))
	assert.Equal(t, opensearchIps, []string(nil))
	assert.Equal(t, postgresqlIps, []string(nil))
	assert.Contains(t, getSingleErrorFromList(errList).Error(), invalidIpAddress)
}

// {OsStatus 10.0.192.172 []  <nil>}
func TestRunFENodeDiaplay(t *testing.T) {
	a2haHabitatAutoTfvars = mockA2haHabitatAutoTfvars
	infra := &AutomateHAInfraDetails{}
	infra.Outputs.AutomatePrivateIps.Value = []string{ValidIP1}
	infra.Outputs.ChefServerPrivateIps.Value = []string{ValidIP2}
	ss := NewStatusSummary(infra, FeStatus{}, BeStatus{}, 10, time.Second, &StatusSummaryCmdFlags{}, &MockRemoteCmdExecutor{
		ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
			if strings.Contains(strings.Join(nodeMap.Automate.CmdInputs.NodeIps, ","), ValidIP1) {

				return map[string][]*CmdResult{
					ValidIP1: {
						&CmdResult{
							ScriptName:  "OsStatus",
							HostIP:      ValidIP1,
							OutputFiles: []string{},
							Error:       nil,
							Output:      `{"cluster_name":"opensearch","status":"green","timed_out":false,"number_of_nodes":3,"number_of_data_nodes":3,"discovered_master":true,"active_primary_shards":35,"active_shards":74,"relocating_shards":0,"initializing_shards":0,"unassigned_shards":0,"delayed_unassigned_shards":0,"number_of_pending_tasks":0,"number_of_in_flight_fetch":0,"task_max_waiting_in_queue_millis":0,"active_shards_percent_as_number":100.0}`,
						},
						&CmdResult{
							ScriptName:  "Status",
							HostIP:      ValidIP1,
							OutputFiles: []string{},
							Error:       nil,
							Output:      ``,
						},
					},
				}, nil
				// displayFEAutomate, nil
			}
			return map[string][]*CmdResult{
				ValidIP2: {
					&CmdResult{
						ScriptName:  "OsStatus",
						HostIP:      ValidIP2,
						OutputFiles: []string{},
						Error:       nil,
						Output:      `{"cluster_name":"opensearch","status":"green","timed_out":false,"number_of_nodes":3,"number_of_data_nodes":3,"discovered_master":true,"active_primary_shards":35,"active_shards":74,"relocating_shards":0,"initializing_shards":0,"unassigned_shards":0,"delayed_unassigned_shards":0,"number_of_pending_tasks":0,"number_of_in_flight_fetch":0,"task_max_waiting_in_queue_millis":0,"active_shards_percent_as_number":100.0}`,
					},
					&CmdResult{
						ScriptName:  "Status",
						HostIP:      ValidIP2,
						OutputFiles: []string{},
						Error:       nil,
						Output:      ``,
					},
				},
			}, nil
		},
	})
	err := ss.Prepare()
	assert.NoError(t, err)
	fe := ss.ShowFEStatus()
	assert.Contains(t, fe, displayFEAutomate)
	assert.Contains(t, fe, displayFECS)

}

func getMockSSHUtilRunSummary(sshConfig *SSHConfig, CFTRError error, CSECORError error) *MockSSHUtilsImpl {
	return &MockSSHUtilsImpl{
		getSSHConfigFunc: func() *SSHConfig {
			return sshConfig
		},
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			if strings.Contains(remoteCommands, "/default/health --header") {
				return `{"status":"OK","stdout":"","stderr":""}`, CSECORError
			}
			if strings.Contains(remoteCommands, "/default --header") {
				return `{
					"process": {
						"pid": 4173,
						"state": "up",
						"state_entered": 0
					},
					"sys": {
						"version": "1.6.521/20220603161331",
						"member_id": "7b945d6006e34463a1901b3d0a997369",
						"ip": "10.1.0.16",
						"hostname": "ip-10-1-0-16.ap-south-1.compute.internal",
						"gossip_ip": "0.0.0.0",
						"gossip_port": 9638,
						"ctl_gateway_ip": "127.0.0.1",
						"ctl_gateway_port": 9632,
						"http_gateway_ip": "0.0.0.0",
						"http_gateway_port": 9631,
						"permanent": true
					}
				}`, CSECORError
			}
			if strings.Contains(remoteCommands, "/census --header") {
				return `{
					"changed": true,
					"census_groups": {
						"automate-ha-postgresql.default": {
							"service_group": "automate-ha-postgresql.default",
							"population": {
								"7b945d6006e34463a1901b3d0a997369": {
									"member_id": "7b945d6006e34463a1901b3d0a997369",
									"leader": true,
									"follower": false,
									"update_leader": false,
									"update_follower": false
							}
							}
						},
						"automate-ha-opensearch.default": {
							"service_group": "automate-ha-opensearch.default",
							"election_status": "None",
							"update_election_status": "None",
							"leader_id": null,
							"service_config": null,
							"local_member_id": "7b945d6006e34463a1901b3d0a997369",
							"population": {
								"7b945d6006e34463a1901b3d0a997369": {
									"member_id": "29e25e5164644ebbb1475fdf12468960",
									"leader": false,
									"follower": false,
									"update_leader": false,
									"update_follower": false
								}
							}
						}
					}
				}`, CSECORError
			}
			if strings.Contains(remoteCommands, "/_cluster/health") {
				return `{
					"cluster_name": "opensearch",
					"status": "green",
					"timed_out": false,
					"number_of_nodes": 3,
					"number_of_data_nodes": 3,
					"discovered_master": true,
					"active_primary_shards": 50,
					"active_shards": 104,
					"relocating_shards": 0,
					"initializing_shards": 0,
					"unassigned_shards": 0,
					"delayed_unassigned_shards": 0,
					"number_of_pending_tasks": 0,
					"number_of_in_flight_fetch": 0,
					"task_max_waiting_in_queue_millis": 0,
					"active_shards_percent_as_number": 100.0
				}`, CSECORError
			}
			return "", CSECORError
		},
		setSSHConfigFunc: func(sshConfig *SSHConfig) {
			// No return for this function
		},
	}
}

func TestGetFollowerLag(t *testing.T) {
	a2haHabitatAutoTfvars = mockA2haHabitatAutoTfvars

	testCases := []struct {
		description string
		beOutput    map[string][]*CmdResult
		expectedErr error
		ip          string
		wantErr     bool
	}{
		{
			description: "Showing Lag with No Error for Follower node",
			beOutput: map[string][]*CmdResult{
				ValidIP7: {
					&CmdResult{
						ScriptName:  censusDetails,
						HostIP:      ValidIP7,
						OutputFiles: []string{},
						Output: `{
							"changed": false,
							"census_groups": {
								"automate-ha-postgresql.default": {
									"population": {
										"8cfc79a0be3c4e74ab717bac25a1d185": {
											"leader": false,
											"follower": true
											}
										}
									}
								}
							}`,
						Error: nil,
					},
					&CmdResult{
						ScriptName:  defaultServiceDetails,
						HostIP:      ValidIP7,
						OutputFiles: []string{},
						Output: `{
							"process": { "pid": 2760, "state": "up", "state_entered": 1689602436 },
							"sys": {
								"member_id": "8cfc79a0be3c4e74ab717bac25a1d185"
							}
						}`,
						Error: nil,
					},
					&CmdResult{
						ScriptName:  defaultServiceHealthDetails,
						HostIP:      ValidIP7,
						OutputFiles: []string{},
						Output:      `{"status":"OK","stdout":"Primary server is available at 10.0.192.111 with uptime: 0s\nLocal replica 10.0.192.209 is 0 bytes and 1 seconds behind the primary\n","stderr":""}`,
						Error:       nil,
					},
				},
			},
			ip:          ValidIP7,
			expectedErr: nil,
			wantErr:     false,
		},
		{
			description: "Showing Lag with Parsing Error",
			beOutput: map[string][]*CmdResult{
				ValidIP7: {
					&CmdResult{
						ScriptName:  censusDetails,
						HostIP:      ValidIP7,
						OutputFiles: []string{},
						Output: `{
							"changed": false,
							"census_groups": {
								"automate-ha-postgresql.default": {
									"population": {
										"8cfc79a0be3c4e74ab717bac25a1d185": {
											"leader": false,
											"follower": true
											
										}
									}
								}
							}`,
						Error: nil,
					},
					&CmdResult{
						ScriptName:  defaultServiceDetails,
						HostIP:      ValidIP7,
						OutputFiles: []string{},
						Output: `{
							"process": { "pid": 2760, "state": "up", "state_entered": 1689602436 ,
							"sys": {
								"member_id": "8cfc79a0be3c4e74ab717bac25a1d185"
							}
						}`,
						Error: nil,
					},
					&CmdResult{
						ScriptName:  defaultServiceHealthDetails,
						HostIP:      ValidIP7,
						OutputFiles: []string{},
						Output:      `{"status":"OK","stdout":"Primary server is available at 10.0.192.111 with uptime: 0s\nLocal replica 10.0.192.209 is 0 bytes and 1 seconds behind the primary\n","stderr":"}`,
						Error:       nil,
					},
				},
			},
			ip:          ValidIP7,
			expectedErr: errors.New("ERROR"),
			wantErr:     true,
		},
	}

	for _, test := range testCases {

		infra := &AutomateHAInfraDetails{}
		infra.Outputs.PostgresqlPrivateIps.Value = []string{test.ip}
		ss := NewStatusSummary(infra, FeStatus{}, BeStatus{}, 10, time.Second, &StatusSummaryCmdFlags{
			node:         test.ip,
			isPostgresql: true,
		}, &MockRemoteCmdExecutor{
			ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
				return test.beOutput, nil
			},
		})

		err := ss.Prepare()
		assert.NoError(t, err)
		be := ss.ShowBEStatus()

		if test.wantErr {
			assert.Contains(t, be, ValidIP7)
			assert.Contains(t, be, test.expectedErr.Error())

		} else {
			assert.Contains(t, be, ValidIP7)
			assert.Contains(t, be, "0 bytes and 1 seconds")
			assert.Contains(t, be, "up (pid: 2760)")
			assert.Contains(t, be, "postgresql")
		}
	}
}

func getMockStatusSummary() StatusSummary {
	infra := &AutomateHAInfraDetails{}
	infra.Outputs.PostgresqlPrivateIps.Value = []string{"192.168.0.1", "192.168.0.2", "192.168.0.3"}
	beOutput := make(map[string][]*CmdResult)
	ss := NewStatusSummary(infra, FeStatus{}, BeStatus{}, 10, time.Second, &StatusSummaryCmdFlags{
		isPostgresql: true,
	}, &MockRemoteCmdExecutor{
		ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
			return beOutput, nil
		},
	})
	return ss
}
