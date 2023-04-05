package main

import (
	"fmt"
	"strings"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
)

func TestCheckIPAddressesFromInfra(t *testing.T) {
	infra := &AutomteHAInfraDetails{}
	infra.Outputs.AutomatePrivateIps.Value = []string{ValidIP, ValidIP1}
	infra.Outputs.ChefServerPrivateIps.Value = []string{ValidIP2, ValidIP3}
	infra.Outputs.OpensearchPrivateIps.Value = []string{ValidIP4, ValidIP5, ValidIP6}
	infra.Outputs.PostgresqlPrivateIps.Value = []string{ValidIP7, ValidIP8, ValidIP9}
	ss := NewStatusSummary(infra, FeStatus{}, BeStatus{}, 10, time.Second, &StatusSummaryCmdFlags{}, getMockSSHUtilRunSummary(&SSHConfig{}, nil, nil))

	automateIps, chefServerIps, opensearchIps, postgresqlIps, errList := ss.(*Summary).getIPAddressesFromFlagOrInfra()
	assert.Equal(t, errList.Len(), 0)
	assert.Equal(t, automateIps, []string{ValidIP, ValidIP1})
	assert.Equal(t, chefServerIps, []string{ValidIP2, ValidIP3})
	assert.Equal(t, opensearchIps, []string{ValidIP4, ValidIP5, ValidIP6})
	assert.Equal(t, postgresqlIps, []string{ValidIP7, ValidIP8, ValidIP9})
}

func TestCheckIPAddressesFromCmd(t *testing.T) {
	infra := &AutomteHAInfraDetails{}
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
	}, getMockSSHUtilRunSummary(&SSHConfig{}, nil, nil))

	automateIps, chefServerIps, opensearchIps, postgresqlIps, errList := ss.(*Summary).getIPAddressesFromFlagOrInfra()
	assert.Equal(t, errList.Len(), 0)
	assert.Equal(t, automateIps, []string{ValidIP})
	assert.Equal(t, chefServerIps, []string{ValidIP3})
	assert.Equal(t, opensearchIps, []string{ValidIP5})
	assert.Equal(t, postgresqlIps, []string{ValidIP8})
}

func TestCheckIPAddressesByServicesAndIpFromFlag(t *testing.T) {
	infra := &AutomteHAInfraDetails{}
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
	}, getMockSSHUtilRunSummary(&SSHConfig{}, nil, nil))

	automateIps, chefServerIps, opensearchIps, postgresqlIps, errList := ss.(*Summary).getIPAddressesFromFlagOrInfra()
	assert.Equal(t, errList.Len(), 0)
	assert.Equal(t, automateIps, []string{ValidIP})
	assert.Equal(t, chefServerIps, []string{ValidIP3})
	assert.Equal(t, opensearchIps, []string{ValidIP5})
	assert.Equal(t, postgresqlIps, []string{ValidIP8})
}

func TestCheckIPAddressesOnlyByServices(t *testing.T) {
	infra := &AutomteHAInfraDetails{}
	infra.Outputs.AutomatePrivateIps.Value = []string{ValidIP, ValidIP1}
	infra.Outputs.ChefServerPrivateIps.Value = []string{ValidIP2, ValidIP3}
	infra.Outputs.OpensearchPrivateIps.Value = []string{ValidIP4, ValidIP5, ValidIP6}
	infra.Outputs.PostgresqlPrivateIps.Value = []string{ValidIP7, ValidIP8, ValidIP9}

	ss := NewStatusSummary(infra, FeStatus{}, BeStatus{}, 10, time.Second, &StatusSummaryCmdFlags{
		isAutomate:   true,
		isChefServer: true,
		isOpenSearch: true,
		isPostgresql: true,
	}, getMockSSHUtilRunSummary(&SSHConfig{}, nil, nil))

	automateIps, chefServerIps, opensearchIps, postgresqlIps, errList := ss.(*Summary).getIPAddressesFromFlagOrInfra()
	assert.Equal(t, errList.Len(), 0)
	assert.Equal(t, automateIps, []string{ValidIP, ValidIP1})
	assert.Equal(t, chefServerIps, []string{ValidIP2, ValidIP3})
	assert.Equal(t, opensearchIps, []string{ValidIP4, ValidIP5, ValidIP6})
	assert.Equal(t, postgresqlIps, []string{ValidIP7, ValidIP8, ValidIP9})
}

func TestCheckIPAddressesError(t *testing.T) {
	infra := &AutomteHAInfraDetails{}
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
	}, getMockSSHUtilRunSummary(&SSHConfig{}, nil, nil))

	_, _, _, _, errList := ss.(*Summary).getIPAddressesFromFlagOrInfra()
	assert.Equal(t, errList.Len(), 1)
	assert.Contains(t, getSingleErrorFromList(errList).Error(), "\nList of  ip address not found [127.0.0.1 127.0.0.2]")
}

func TestCheckIPAddressesValidation(t *testing.T) {
	infra := &AutomteHAInfraDetails{}
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
	}, getMockSSHUtilRunSummary(&SSHConfig{}, nil, nil))

	automateIps, chefServerIps, opensearchIps, postgresqlIps, errList := ss.(*Summary).getIPAddressesFromFlagOrInfra()
	assert.Equal(t, errList.Len(), 17)
	assert.Equal(t, automateIps, []string(nil))
	assert.Equal(t, chefServerIps, []string(nil))
	assert.Equal(t, opensearchIps, []string(nil))
	assert.Equal(t, postgresqlIps, []string(nil))
	assert.Contains(t, getSingleErrorFromList(errList).Error(), "\nIncorrect Automate IP address format for ip 127.0.0IP address validation failed\nIncorrect Automate IP address format for ip 127.0.1IP address validation failed\nIncorrect Automate IP address format for ip 127.0.2IP address validation failed\nIncorrect Automate IP address format for ip 127.0.3IP address validation failed\nIncorrect chef-server IP address format for ip 127.0.0IP address validation failed\nIncorrect chef-server IP address format for ip 127.0.1IP address validation failed\nIncorrect chef-server IP address format for ip 127.0.2IP address validation failed\nIncorrect chef-server IP address format for ip 127.0.3IP address validation failed\nIncorrect OpenSearch IP address format for ip 127.0.0IP address validation failed\nIncorrect OpenSearch IP address format for ip 127.0.1IP address validation failed\nIncorrect OpenSearch IP address format for ip 127.0.2IP address validation failed\nIncorrect OpenSearch IP address format for ip 127.0.3IP address validation failed\nIncorrect postgres IP address format for ip 127.0.0IP address validation failed\nIncorrect postgres IP address format for ip 127.0.1IP address validation failed\nIncorrect postgres IP address format for ip 127.0.2IP address validation failed\nIncorrect postgres IP address format for ip 127.0.3IP address validation failed\nList of  ip address not found [127.0.0 127.0.1 127.0.2 127.0.3]")
}
func TestRunFENodeDiaplay(t *testing.T) {
	a2haHabitatAutoTfvars = "../../pkg/testfiles/a2ha_habitat.auto.tfvars"
	infra := &AutomteHAInfraDetails{}
	infra.Outputs.AutomatePrivateIps.Value = []string{ValidIP, ValidIP1}
	infra.Outputs.ChefServerPrivateIps.Value = []string{ValidIP2, ValidIP3}
	sshUtil := getMockSSHUtilRunSummary(&SSHConfig{}, nil, nil)
	ss := NewStatusSummary(infra, FeStatus{}, BeStatus{}, 10, time.Second, &StatusSummaryCmdFlags{}, sshUtil)
	err := ss.Prepare()
	assert.NoError(t, err)
	fe := ss.ShowFEStatus()
	assert.Equal(t, fe, "+-------------+------------+--------+-------------------------+\n| NAME        | IP ADDRESS | STATUS | OPENSEARCH              |\n+-------------+------------+--------+-------------------------+\n| automate    |            | OK     | \"green\" (Active: 100.0) |\n| chef-server |            | OK     | \"green\" (Active: 100.0) |\n+-------------+------------+--------+-------------------------+")
}

func TestRunBENode(t *testing.T) {
	a2haHabitatAutoTfvars = "../../pkg/testfiles/a2ha_habitat.auto.tfvars"
	infra := &AutomteHAInfraDetails{}
	infra.Outputs.OpensearchPrivateIps.Value = []string{ValidIP4, ValidIP5, ValidIP6}
	infra.Outputs.PostgresqlPrivateIps.Value = []string{ValidIP7, ValidIP8, ValidIP9}
	ss := NewStatusSummary(infra, FeStatus{}, BeStatus{}, 10, time.Second, &StatusSummaryCmdFlags{}, getMockSSHUtilRunSummary(&SSHConfig{}, nil, nil))
	err := ss.Prepare()
	assert.NoError(t, err)
}
func TestRunBENodeDiaplay(t *testing.T) {
	a2haHabitatAutoTfvars = "../../pkg/testfiles/a2ha_habitat.auto.tfvars"
	infra := &AutomteHAInfraDetails{}
	infra.Outputs.OpensearchPrivateIps.Value = []string{ValidIP4, ValidIP5, ValidIP6}
	infra.Outputs.PostgresqlPrivateIps.Value = []string{ValidIP7, ValidIP8, ValidIP9}
	ss := NewStatusSummary(infra, FeStatus{}, BeStatus{}, 10, time.Second, &StatusSummaryCmdFlags{}, getMockSSHUtilRunSummary(&SSHConfig{}, nil, nil))
	// Mock the time to a specific time
	mockTime := time.Date(2023, 3, 15, 12, 0, 0, 0, time.UTC)
	nowFunc = func() time.Time { return mockTime }
	// c.Execute()
	err := ss.Prepare()
	assert.NoError(t, err)
	be := ss.ShowBEStatus()
	assert.Equal(t, be, "+------------+------------+--------+----------------+------------------+---------+\n| NAME       | IP ADDRESS | HEALTH | PROCESS        | UPTIME           | ROLE    |\n+------------+------------+--------+----------------+------------------+---------+\n| opensearch |            | OK     | up (pid: 4173) | 19431d 12h 0m 0s | OS node |\n| postgresql |            | OK     | up (pid: 4173) | 19431d 12h 0m 0s | Leader  |\n+------------+------------+--------+----------------+------------------+---------+")
}

func getMockSSHUtilRunSummary(sshConfig *SSHConfig, CFTRError error, CSECORError error) *MockSSHUtilsImpl {
	return &MockSSHUtilsImpl{
		getSSHConfigFunc: func() *SSHConfig {
			return sshConfig
		},
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			fmt.Println(remoteCommands, "remoteCommands")
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
	}
}
