package batchcheckservice

import (
	"encoding/json"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/stretchr/testify/assert"
)

func TestStatusService(t *testing.T) {
	t.Skip("Skipping this test for now")
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
	expectedResponse := `[{"node_type":"","ip":"1.2.3.4","tests":[{"passed":false,"msg":"Hardware Resource Count Check","check":"hardware-resource-count","checks":[{"title":"hardware-resource-check-1","passed":false,"success_msg":"","error_msg":"","resolution_msg":""},{"title":"hardware-resource-check-2","passed":false,"success_msg":"","error_msg":"","resolution_msg":""}]},{"passed":false,"msg":"ssh-user-check","check":"ssh-user","checks":[{"title":"ssh-user-check-1","passed":false,"success_msg":"","error_msg":"","resolution_msg":""},{"title":"ssh-user-check-2","passed":false,"success_msg":"","error_msg":"","resolution_msg":""}]},{"passed":false,"msg":"Firewall-check","check":"firewall","checks":[{"title":"Firewall-check-1","passed":false,"success_msg":"","error_msg":"","resolution_msg":""},{"title":"Firewall-check-2","passed":false,"success_msg":"","error_msg":"","resolution_msg":""}]},{"passed":false,"msg":"fqdn-check","check":"fqdn","checks":[{"title":"Fqdn-check-1","passed":false,"success_msg":"","error_msg":"","resolution_msg":""},{"title":"Fqdn-check-2","passed":false,"success_msg":"","error_msg":"","resolution_msg":""}]}]},{"node_type":"","ip":"1.2.4.5","tests":[{"passed":true,"msg":"Hardware Resource Count Check","check":"hardware-resource-count","checks":[{"title":"hardware-resource-check-1","passed":false,"success_msg":"","error_msg":"","resolution_msg":""},{"title":"hardware-resource-check-2","passed":false,"success_msg":"","error_msg":"","resolution_msg":""}]},{"passed":true,"msg":"ssh-user-check","check":"ssh-user","checks":[{"title":"ssh-user-check-1","passed":false,"success_msg":"","error_msg":"","resolution_msg":""},{"title":"ssh-user-check-2","passed":false,"success_msg":"","error_msg":"","resolution_msg":""}]},{"passed":true,"msg":"Firewall-check","check":"firewall","checks":[{"title":"Firewall-check-1","passed":false,"success_msg":"","error_msg":"","resolution_msg":""},{"title":"Firewall-check-2","passed":false,"success_msg":"","error_msg":"","resolution_msg":""}]},{"passed":true,"msg":"fqdn-check","check":"fqdn","checks":[{"title":"Fqdn-check-1","passed":false,"success_msg":"","error_msg":"","resolution_msg":""},{"title":"Fqdn-check-2","passed":false,"success_msg":"","error_msg":"","resolution_msg":""}]}]}]`
	resp := ss.BatchCheck([]string{constants.HARDWARE_RESOURCE_COUNT, constants.FIREWALL, constants.FQDN, constants.SSH_USER}, models.Config{})
	assert.Equal(t, resp.Status, "SUCCESS")
	b, _ := json.Marshal(resp.Result)
	assert.Equal(t, len(resp.Result), 2)
	assert.Equal(t, string(b), expectedResponse)
	assert.JSONEq(t, expectedResponse, string(b))

}
