package batchcheckservice

import (
	"encoding/json"
	"testing"

	"github.com/gofiber/fiber"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/stretchr/testify/assert"
)

func TestBatchCheckService(t *testing.T) {
	tests := []struct {
		description                      string
		hardwareCheckMockedResponse      []models.CheckTriggerResponse
		sshUserCheckMockedResponse       []models.CheckTriggerResponse
		externalOpenSearchMockedResponse []models.CheckTriggerResponse
		externalPostgresMockedResponse   []models.CheckTriggerResponse
		chefServerIpArray                []string
		expectedResponseForAutomateIp    string
		expectedResponseFromChefServerIp string
		expectedResponseForPostgresIp1   string
		expectedResponseForPostgresIp2   string
		expectedResponseForOpenSearchIp1 string
		expectedResponseForOpenSearchIp2 string
	}{
		{
			description:       "Batch check server success with automate and chef-server on same IP",
			chefServerIpArray: []string{"1.2.3.4"},
			hardwareCheckMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.4",
					NodeType: "automate",
					Result: models.ApiResult{
						Passed:  true,
						Check:   "hardware-resource-count",
						Message: "Hardware Resource Count Check",
						Checks: []models.Checks{
							{
								Title: "hardware-resource-check-1",
							},
							{
								Title: "hardware-resource-check-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.4",
					NodeType: "chef-infra-server",
					Result: models.ApiResult{
						Passed:  true,
						Check:   "hardware-resource-count",
						Message: "Hardware Resource Count Check",
						Checks: []models.Checks{
							{
								Title: "hardware-resource-check-1",
							},
							{
								Title: "hardware-resource-check-2",
							},
						},
					},
				},
			},
			sshUserCheckMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.4",
					NodeType: "automate",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.SSH_USER,
						Message: "ssh-user-check",
						Checks: []models.Checks{
							{
								Title: "ssh-user-check-1",
							},
							{
								Title: "ssh-user-check-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.4",
					NodeType: "chef-infra-server",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.SSH_USER,
						Message: "ssh-user-check",
						Checks: []models.Checks{
							{
								Title: "ssh-user-check-1",
							},
							{
								Title: "ssh-user-check-2",
							},
						},
					},
				},
			},
			externalOpenSearchMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.5",
					NodeType: "opensearch",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.EXTERNAL_OPENSEARCH,
						Message: "external-opensearch-check",
						Checks: []models.Checks{
							{
								Title: "external-opensearch-1",
							},
							{
								Title: "external-opensearch-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.6",
					NodeType: "opensearch",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.EXTERNAL_OPENSEARCH,
						Message: "external-opensearch-check",
						Checks: []models.Checks{
							{
								Title: "external-opensearch-check-1",
							},
							{
								Title: "external-opensearch-check-2",
							},
						},
					},
				},
			},
			externalPostgresMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.7",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.EXTERNAL_POSTGRESQL,
						Message: "external-postgresql-check",
						Checks: []models.Checks{
							{
								Title: "external-postgresql-1",
							},
							{
								Title: "external-postgresql-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.8",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.EXTERNAL_POSTGRESQL,
						Message: "external-postgresql-check",
						Checks: []models.Checks{
							{
								Title: "external-postgresql-check-1",
							},
							{
								Title: "external-postgresql-check-2",
							},
						},
					},
				},
			},
			expectedResponseForAutomateIp:    "{\"node_type\":\"automate\",\"ip\":\"1.2.3.4\",\"tests\":[{\"passed\":true,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[{\"title\":\"hardware-resource-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"hardware-resource-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":false,\"msg\":\"ssh-user-check\",\"check\":\"ssh-user\",\"checks\":[{\"title\":\"ssh-user-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"ssh-user-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseFromChefServerIp: "{\"node_type\":\"chef-infra-server\",\"ip\":\"1.2.3.4\",\"tests\":[{\"passed\":true,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[{\"title\":\"hardware-resource-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"hardware-resource-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":true,\"msg\":\"ssh-user-check\",\"check\":\"ssh-user\",\"checks\":[{\"title\":\"ssh-user-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"ssh-user-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForPostgresIp1:   "{\"node_type\":\"postgresql\",\"ip\":\"1.2.3.7\",\"tests\":[{\"passed\":false,\"msg\":\"external-postgresql-check\",\"check\":\"external-postgresql\",\"checks\":[{\"title\":\"external-postgresql-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-postgresql-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForPostgresIp2:   "{\"node_type\":\"postgresql\",\"ip\":\"1.2.3.8\",\"tests\":[{\"passed\":true,\"msg\":\"external-postgresql-check\",\"check\":\"external-postgresql\",\"checks\":[{\"title\":\"external-postgresql-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-postgresql-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForOpenSearchIp1: "{\"node_type\":\"opensearch\",\"ip\":\"1.2.3.5\",\"tests\":[{\"passed\":false,\"msg\":\"external-opensearch-check\",\"check\":\"external-opensearch\",\"checks\":[{\"title\":\"external-opensearch-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-opensearch-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForOpenSearchIp2: "{\"node_type\":\"opensearch\",\"ip\":\"1.2.3.6\",\"tests\":[{\"passed\":true,\"msg\":\"external-opensearch-check\",\"check\":\"external-opensearch\",\"checks\":[{\"title\":\"external-opensearch-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-opensearch-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
		},
		{
			description:       "Batch check server success with nodes on different ip",
			chefServerIpArray: []string{"1.2.8.4"},
			hardwareCheckMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.4",
					NodeType: "automate",
					Result: models.ApiResult{
						Passed:  true,
						Check:   "hardware-resource-count",
						Message: "Hardware Resource Count Check",
						Checks: []models.Checks{
							{
								Title: "hardware-resource-check-1",
							},
							{
								Title: "hardware-resource-check-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.8.4",
					NodeType: "chef-infra-server",
					Result: models.ApiResult{
						Passed:  true,
						Check:   "hardware-resource-count",
						Message: "Hardware Resource Count Check",
						Checks: []models.Checks{
							{
								Title: "hardware-resource-check-1",
							},
							{
								Title: "hardware-resource-check-2",
							},
						},
					},
				},
			},
			sshUserCheckMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.4",
					NodeType: "automate",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.SSH_USER,
						Message: "ssh-user-check",
						Checks: []models.Checks{
							{
								Title: "ssh-user-check-1",
							},
							{
								Title: "ssh-user-check-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.8.4",
					NodeType: "chef-infra-server",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.SSH_USER,
						Message: "ssh-user-check",
						Checks: []models.Checks{
							{
								Title: "ssh-user-check-1",
							},
							{
								Title: "ssh-user-check-2",
							},
						},
					},
				},
			},
			externalOpenSearchMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.5",
					NodeType: "opensearch",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.EXTERNAL_OPENSEARCH,
						Message: "external-opensearch-check",
						Checks: []models.Checks{
							{
								Title: "external-opensearch-1",
							},
							{
								Title: "external-opensearch-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.6",
					NodeType: "opensearch",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.EXTERNAL_OPENSEARCH,
						Message: "external-opensearch-check",
						Checks: []models.Checks{
							{
								Title: "external-opensearch-check-1",
							},
							{
								Title: "external-opensearch-check-2",
							},
						},
					},
				},
			},
			externalPostgresMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.7",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.EXTERNAL_POSTGRESQL,
						Message: "external-postgresql-check",
						Checks: []models.Checks{
							{
								Title: "external-postgresql-1",
							},
							{
								Title: "external-postgresql-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.8",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.EXTERNAL_POSTGRESQL,
						Message: "external-postgresql-check",
						Checks: []models.Checks{
							{
								Title: "external-postgresql-check-1",
							},
							{
								Title: "external-postgresql-check-2",
							},
						},
					},
				},
			},
			expectedResponseForAutomateIp:    "{\"node_type\":\"automate\",\"ip\":\"1.2.3.4\",\"tests\":[{\"passed\":true,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[{\"title\":\"hardware-resource-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"hardware-resource-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":false,\"msg\":\"ssh-user-check\",\"check\":\"ssh-user\",\"checks\":[{\"title\":\"ssh-user-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"ssh-user-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseFromChefServerIp: "{\"node_type\":\"chef-infra-server\",\"ip\":\"1.2.8.4\",\"tests\":[{\"passed\":true,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[{\"title\":\"hardware-resource-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"hardware-resource-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":true,\"msg\":\"ssh-user-check\",\"check\":\"ssh-user\",\"checks\":[{\"title\":\"ssh-user-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"ssh-user-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForPostgresIp1:   "{\"node_type\":\"postgresql\",\"ip\":\"1.2.3.7\",\"tests\":[{\"passed\":false,\"msg\":\"external-postgresql-check\",\"check\":\"external-postgresql\",\"checks\":[{\"title\":\"external-postgresql-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-postgresql-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForPostgresIp2:   "{\"node_type\":\"postgresql\",\"ip\":\"1.2.3.8\",\"tests\":[{\"passed\":true,\"msg\":\"external-postgresql-check\",\"check\":\"external-postgresql\",\"checks\":[{\"title\":\"external-postgresql-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-postgresql-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForOpenSearchIp1: "{\"node_type\":\"opensearch\",\"ip\":\"1.2.3.5\",\"tests\":[{\"passed\":false,\"msg\":\"external-opensearch-check\",\"check\":\"external-opensearch\",\"checks\":[{\"title\":\"external-opensearch-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-opensearch-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForOpenSearchIp2: "{\"node_type\":\"opensearch\",\"ip\":\"1.2.3.6\",\"tests\":[{\"passed\":true,\"msg\":\"external-opensearch-check\",\"check\":\"external-opensearch\",\"checks\":[{\"title\":\"external-opensearch-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-opensearch-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
		},
		{
			description:       "Batch check server success with error on some nodes",
			chefServerIpArray: []string{"1.2.8.4"},
			hardwareCheckMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Failed",
					Host:     "1.2.3.4",
					NodeType: "automate",
					Result: models.ApiResult{
						Passed:  true,
						Check:   "hardware-resource-count",
						Message: "Hardware Resource Count Check",
						Checks:  []models.Checks{},
						Error:   fiber.NewError(fiber.StatusServiceUnavailable, "Error while Performing Hardware resource count check from batch Check API"),
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.8.4",
					NodeType: "chef-infra-server",
					Result: models.ApiResult{
						Passed:  true,
						Check:   "hardware-resource-count",
						Message: "Hardware Resource Count Check",
						Checks: []models.Checks{
							{
								Title: "hardware-resource-check-1",
							},
							{
								Title: "hardware-resource-check-2",
							},
						},
					},
				},
			},
			sshUserCheckMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.4",
					NodeType: "automate",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.SSH_USER,
						Message: "ssh-user-check",
						Checks: []models.Checks{
							{
								Title: "ssh-user-check-1",
							},
							{
								Title: "ssh-user-check-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.8.4",
					NodeType: "chef-infra-server",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.SSH_USER,
						Message: "ssh-user-check",
						Checks: []models.Checks{
							{
								Title: "ssh-user-check-1",
							},
							{
								Title: "ssh-user-check-2",
							},
						},
					},
				},
			},
			externalOpenSearchMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.5",
					NodeType: "opensearch",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.EXTERNAL_OPENSEARCH,
						Message: "external-opensearch-check",
						Checks: []models.Checks{
							{
								Title: "external-opensearch-1",
							},
							{
								Title: "external-opensearch-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.6",
					NodeType: "opensearch",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.EXTERNAL_OPENSEARCH,
						Message: "external-opensearch-check",
						Checks: []models.Checks{
							{
								Title: "external-opensearch-check-1",
							},
							{
								Title: "external-opensearch-check-2",
							},
						},
					},
				},
			},
			externalPostgresMockedResponse: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.7",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.EXTERNAL_POSTGRESQL,
						Message: "external-postgresql-check",
						Checks: []models.Checks{
							{
								Title: "external-postgresql-1",
							},
							{
								Title: "external-postgresql-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.8",
					NodeType: "postgresql",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.EXTERNAL_POSTGRESQL,
						Message: "external-postgresql-check",
						Checks: []models.Checks{
							{
								Title: "external-postgresql-check-1",
							},
							{
								Title: "external-postgresql-check-2",
							},
						},
					},
				},
			},
			expectedResponseForAutomateIp:    "{\"node_type\":\"automate\",\"ip\":\"1.2.3.4\",\"tests\":[{\"passed\":true,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[],\"error\":{\"code\":503,\"message\":\"Error while Performing Hardware resource count check from batch Check API\"}},{\"passed\":false,\"msg\":\"ssh-user-check\",\"check\":\"ssh-user\",\"checks\":[{\"title\":\"ssh-user-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"ssh-user-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseFromChefServerIp: "{\"node_type\":\"chef-infra-server\",\"ip\":\"1.2.8.4\",\"tests\":[{\"passed\":true,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[{\"title\":\"hardware-resource-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"hardware-resource-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":true,\"msg\":\"ssh-user-check\",\"check\":\"ssh-user\",\"checks\":[{\"title\":\"ssh-user-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"ssh-user-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForPostgresIp1:   "{\"node_type\":\"postgresql\",\"ip\":\"1.2.3.7\",\"tests\":[{\"passed\":false,\"msg\":\"external-postgresql-check\",\"check\":\"external-postgresql\",\"checks\":[{\"title\":\"external-postgresql-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-postgresql-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForPostgresIp2:   "{\"node_type\":\"postgresql\",\"ip\":\"1.2.3.8\",\"tests\":[{\"passed\":true,\"msg\":\"external-postgresql-check\",\"check\":\"external-postgresql\",\"checks\":[{\"title\":\"external-postgresql-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-postgresql-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForOpenSearchIp1: "{\"node_type\":\"opensearch\",\"ip\":\"1.2.3.5\",\"tests\":[{\"passed\":false,\"msg\":\"external-opensearch-check\",\"check\":\"external-opensearch\",\"checks\":[{\"title\":\"external-opensearch-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-opensearch-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
			expectedResponseForOpenSearchIp2: "{\"node_type\":\"opensearch\",\"ip\":\"1.2.3.6\",\"tests\":[{\"passed\":true,\"msg\":\"external-opensearch-check\",\"check\":\"external-opensearch\",\"checks\":[{\"title\":\"external-opensearch-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-opensearch-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}",
		},
	}

	for _, test := range tests {
		t.Run(test.description, func(t *testing.T) {
			ss := NewBatchCheckService(trigger.NewCheckTrigger(SetupMockHardwareResourceCountCheck(test.hardwareCheckMockedResponse),
				SetupMockSshUserAccessCheck(test.sshUserCheckMockedResponse),
				SetupMockCertificateCheck(),
				SetupMockExternalOpenSearchCheck(test.externalOpenSearchMockedResponse),
				SetupMockExternalPostgresCheck(test.externalPostgresMockedResponse),
				SetupMockFirewallCheck(),
				SetupMockFqdnCheck(),
				SetupMockNfsBackupConfigCheck(),
				SetupMockOpenSearchS3BucketAccessCheck(),
				SetupMockS3BackupConfigCheck(),
				SetupMockSoftwareVersionCheck(),
				SetupMockSystemResourceCheck(),
				SetupMockSystemUserCheck(),
			))
			resp := ss.BatchCheck([]string{constants.HARDWARE_RESOURCE_COUNT,
				constants.FIREWALL,
				constants.FQDN,
				constants.SSH_USER,
				constants.EXTERNAL_OPENSEARCH,
				constants.EXTERNAL_POSTGRESQL,
				constants.NFS_BACKUP_CONFIG,
				constants.S3_BACKUP_CONFIG,
				constants.SOFTWARE_VERSIONS,
				constants.SYSTEM_RESOURCES,
				constants.CERTIFICATE,
				constants.SYSTEM_USER,
				constants.AWS_OPENSEARCH_S3_BUCKET_ACCESS}, models.Config{
				Hardware: models.Hardware{
					AutomateNodeCount:        1,
					AutomateNodeIps:          []string{"1.2.3.4"},
					ChefInfraServerNodeCount: 1,
					ChefInfraServerNodeIps:   test.chefServerIpArray,
					PostgresqlNodeCount:      1,
					PostgresqlNodeIps:        []string{"1.2.3.7", "1.2.3.8"},
					OpenSearchNodeCount:      1,
					OpenSearchNodeIps:        []string{"1.2.3.5", "1.2.3.6"},
				},
			})

			assert.Equal(t, resp.Status, "SUCCESS")
			assert.Equal(t, len(resp.Result), 6)
			assert.Equal(t, test.expectedResponseForAutomateIp, getResponseForIp(resp.Result, "1.2.3.4", "automate"))
			assert.Equal(t, test.expectedResponseFromChefServerIp, getResponseForIp(resp.Result, test.chefServerIpArray[0], "chef-infra-server"))
			assert.Equal(t, test.expectedResponseForPostgresIp1, getResponseForIp(resp.Result, "1.2.3.7", "postgresql"))
			assert.Equal(t, test.expectedResponseForPostgresIp2, getResponseForIp(resp.Result, "1.2.3.8", "postgresql"))
			assert.Equal(t, test.expectedResponseForOpenSearchIp1, getResponseForIp(resp.Result, "1.2.3.5", "opensearch"))
			assert.Equal(t, test.expectedResponseForOpenSearchIp2, getResponseForIp(resp.Result, "1.2.3.6", "opensearch"))
		})
	}

}

func getResponseForIp(resp []models.BatchCheckResult, ip string, nodeType string) string {
	for _, res := range resp {
		if res.Ip == ip && res.NodeType == nodeType {
			b, _ := json.Marshal(res)
			return string(b)
		}
	}
	return ""
}
