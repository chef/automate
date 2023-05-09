package batchcheckservice

import (
	"encoding/json"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/stretchr/testify/assert"
)

func TestBatchCheckServiceService(t *testing.T) {
	ss := NewBatchCheckService(trigger.NewCheckTrigger(SetupMockHardwareResourceCountCheck(),
		SetupMockSshUserAccessCheck(),
		SetupMockCertificateCheck(),
		SetupMockExternalOpensearchCheck(),
		SetupMockExternalPostgresCheck(),
		SetupMockFirewallCheck(),
		SetupMockFqdnCheck(),
		SetupMockNfsBackupConfigCheck(),
		SetupMockOpensearchS3BucketAccessCheck(),
		SetupMockS3BackupConfigCheck(),
		SetupMockSoftwareVersionCheck(),
		SetupMockSystemResourceCheck(),
		SetupMockSystemUserCheck(),
	))
	expectedResponseForAutomateIp := "{\"node_type\":\"automate\",\"ip\":\"1.2.3.4\",\"tests\":[{\"passed\":false,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[{\"title\":\"hardware-resource-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"hardware-resource-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":false,\"msg\":\"ssh-user-check\",\"check\":\"ssh-user\",\"checks\":[{\"title\":\"ssh-user-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"ssh-user-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":false,\"msg\":\"Firewall-check\",\"check\":\"firewall\",\"checks\":[{\"title\":\"Firewall-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"Firewall-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":false,\"msg\":\"fqdn-check\",\"check\":\"fqdn\",\"checks\":[{\"title\":\"Fqdn-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"Fqdn-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}"
	expectedResponseFromChefServerIp := "{\"node_type\":\"chef_server\",\"ip\":\"1.2.4.5\",\"tests\":[{\"passed\":true,\"msg\":\"Hardware Resource Count Check\",\"check\":\"hardware-resource-count\",\"checks\":[{\"title\":\"hardware-resource-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"hardware-resource-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":true,\"msg\":\"ssh-user-check\",\"check\":\"ssh-user\",\"checks\":[{\"title\":\"ssh-user-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"ssh-user-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":true,\"msg\":\"Firewall-check\",\"check\":\"firewall\",\"checks\":[{\"title\":\"Firewall-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"Firewall-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]},{\"passed\":true,\"msg\":\"fqdn-check\",\"check\":\"fqdn\",\"checks\":[{\"title\":\"Fqdn-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"Fqdn-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}"
	expectedResponseForPostgresIp := "{\"node_type\":\"postgresql\",\"ip\":\"1.2.3.5\",\"tests\":[{\"passed\":false,\"msg\":\"External Opensearch Check\",\"check\":\"external-opensearch\",\"checks\":[{\"title\":\"external-opensearch-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-opensearch-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}"
	expectedResponseForOpenSearchIp := "{\"node_type\":\"opensearch\",\"ip\":\"1.2.3.6\",\"tests\":[{\"passed\":false,\"msg\":\"External Postgres Check\",\"check\":\"external-postgresql\",\"checks\":[{\"title\":\"external-postgresql-check-1\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"external-postgresql-check-2\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}]}"

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
		constants.AWS_OPENSEARCH_S3_BUCKET_ACCESS,
	}, models.Config{
		Hardware: models.Hardware{
			AutomateNodeCount:        1,
			AutomateNodeIps:          []string{"1.2.3.4"},
			ChefInfraServerNodeCount: 1,
			ChefInfraServerNodeIps:   []string{"1.2.4.5"},
			PostgresqlNodeCount:      1,
			PostgresqlNodeIps:        []string{"1.2.3.5"},
			OpenSearchNodeCount:      1,
			OpenSearchNodeIps:        []string{"1.2.3.6"},
		},
	})
	assert.Equal(t, resp.Status, "SUCCESS")
	assert.Equal(t, len(resp.Result), 4)
	assert.Equal(t, expectedResponseForAutomateIp, getResponseForIp(resp.Result, "1.2.3.4"))
	assert.Equal(t, expectedResponseFromChefServerIp, getResponseForIp(resp.Result, "1.2.4.5"))
	assert.Equal(t, expectedResponseForPostgresIp, getResponseForIp(resp.Result, "1.2.3.5"))
	assert.Equal(t, expectedResponseForOpenSearchIp, getResponseForIp(resp.Result, "1.2.3.6"))

}

func getResponseForIp(resp []models.BatchCheckResult, ip string) string {
	for _, res := range resp {
		if res.Ip == ip {
			b, _ := json.Marshal(res)
			return string(b)
		}
	}
	return ""
}
