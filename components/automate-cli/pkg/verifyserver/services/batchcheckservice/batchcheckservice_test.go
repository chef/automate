package batchcheckservice

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/stretchr/testify/assert"
)

func TestStatusService(t *testing.T) {
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

	resp := ss.BatchCheck([]string{constants.HARDWARE_RESOURCE_COUNT, constants.SSH_USER}, models.Config{})
	assert.Equal(t, resp.Status, "SUCCESS")
	assert.Equal(t, len(resp.Result), 2)

}
