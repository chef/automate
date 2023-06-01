package constants

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestGetBastionChecks(t *testing.T) {
	resp := GetBastionChecks()
	assert.Equal(t, len(resp), 3)
	assert.Contains(t, resp, HARDWARE_RESOURCE_COUNT)
	assert.Contains(t, resp, CERTIFICATE)
	assert.Contains(t, resp, SSH_USER)
	assert.NotContains(t, resp, FIREWALL)
}

func TestGetRemoteChecks(t *testing.T) {
	resp := GetRemoteChecks()
	assert.Equal(t, len(resp), 10)
	assert.Contains(t, resp, SYSTEM_RESOURCES)
	assert.Contains(t, resp, SOFTWARE_VERSIONS)
	assert.Contains(t, resp, SYSTEM_USER)
	assert.Contains(t, resp, S3_BACKUP_CONFIG)
	assert.Contains(t, resp, FQDN)
	assert.Contains(t, resp, FIREWALL)
	assert.Contains(t, resp, EXTERNAL_OPENSEARCH)
	assert.Contains(t, resp, AWS_OPENSEARCH_S3_BUCKET_ACCESS)
	assert.Contains(t, resp, EXTERNAL_POSTGRESQL)
	assert.Contains(t, resp, NFS_BACKUP_CONFIG)
}

func TestGetCheckByType(t *testing.T) {
	t.Run("Get NFS_BACKUP_CONFIG_MSG ", func(t *testing.T) {
		resp := GetCheckByType(NFS_BACKUP_CONFIG)
		assert.Equal(t, resp, NFS_BACKUP_CONFIG_MSG)
	})

	t.Run("Get FIREWALL_MSG", func(t *testing.T) {
		resp := GetCheckByType(FIREWALL)
		assert.Equal(t, resp, FIREWALL_MSG)
	})

	t.Run("Get FIREWALL_MSG", func(t *testing.T) {
		resp := GetCheckByType("random-string")
		assert.Equal(t, resp, "")
		assert.Empty(t, resp)
	})

}
