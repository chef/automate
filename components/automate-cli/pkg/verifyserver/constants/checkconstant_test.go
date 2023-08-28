package constants

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestGetBastionChecks(t *testing.T) {
	resp := GetBastionChecks()
	assert.Equal(t, 3, len(resp))
	assert.Contains(t, resp, HARDWARE_RESOURCE_COUNT)
	assert.Contains(t, resp, CERTIFICATE)
	assert.Contains(t, resp, SSH_USER)
	assert.NotContains(t, resp, FIREWALL)
}

func TestGetRemoteChecks(t *testing.T) {
	resp := GetRemoteChecks()
	assert.Equal(t, len(resp), 11)
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
	assert.Contains(t, resp, GCP_BACKUP_CONFIG)
}

func TestGetCheckMessageByName(t *testing.T) {
	t.Run("Get HARDWARE_RESOURCE_COUNT_MSG", func(t *testing.T) {
		resp := GetCheckMessageByName(HARDWARE_RESOURCE_COUNT)
		assert.Equal(t, resp, HARDWARE_RESOURCE_COUNT_MSG)
	})

	t.Run("Get CERTIFICATE_MSG", func(t *testing.T) {
		resp := GetCheckMessageByName(CERTIFICATE)
		assert.Equal(t, resp, CERTIFICATE_MSG)
	})

	t.Run("Get SSH_USER_MSG", func(t *testing.T) {
		resp := GetCheckMessageByName(SSH_USER)
		assert.Equal(t, resp, SSH_USER_MSG)
	})

	t.Run("Get SYSTEM_RESOURCES_MSG", func(t *testing.T) {
		resp := GetCheckMessageByName(SYSTEM_RESOURCES)
		assert.Equal(t, resp, SYSTEM_RESOURCES_MSG)
	})

	t.Run("Get SOFTWARE_VERSIONS_MSG", func(t *testing.T) {
		resp := GetCheckMessageByName(SOFTWARE_VERSIONS)
		assert.Equal(t, resp, SOFTWARE_VERSIONS_MSG)
	})

	t.Run("Get SYSTEM_USER_MSG", func(t *testing.T) {
		resp := GetCheckMessageByName(SYSTEM_USER)
		assert.Equal(t, resp, SYSTEM_USER_MSG)
	})

	t.Run("Get S3_BACKUP_CONFIG_MSG", func(t *testing.T) {
		resp := GetCheckMessageByName(S3_BACKUP_CONFIG)
		assert.Equal(t, resp, S3_BACKUP_CONFIG_MSG)
	})

	t.Run("Get FQDN_MSG", func(t *testing.T) {
		resp := GetCheckMessageByName(FQDN)
		assert.Equal(t, resp, FQDN_MSG)
	})

	t.Run("Get FIREWALL_MSG", func(t *testing.T) {
		resp := GetCheckMessageByName(FIREWALL)
		assert.Equal(t, resp, FIREWALL_MSG)
	})

	t.Run("Get EXTERNAL_OPENSEARCH_MSG", func(t *testing.T) {
		resp := GetCheckMessageByName(EXTERNAL_OPENSEARCH)
		assert.Equal(t, resp, EXTERNAL_OPENSEARCH_MSG)
	})

	t.Run("Get AWS_OPENSEARCH_S3_BUCKET_ACCESS_MSG", func(t *testing.T) {
		resp := GetCheckMessageByName(AWS_OPENSEARCH_S3_BUCKET_ACCESS)
		assert.Equal(t, resp, AWS_OPENSEARCH_S3_BUCKET_ACCESS_MSG)
	})

	t.Run("Get EXTERNAL_POSTGRESQL_MSG", func(t *testing.T) {
		resp := GetCheckMessageByName(EXTERNAL_POSTGRESQL)
		assert.Equal(t, resp, EXTERNAL_POSTGRESQL_MSG)
	})

	t.Run("Get NFS_BACKUP_CONFIG_MSG", func(t *testing.T) {
		resp := GetCheckMessageByName(NFS_BACKUP_CONFIG)
		assert.Equal(t, resp, NFS_BACKUP_CONFIG_MSG)
	})

	t.Run("Get GCP_BACKUP_CONFIG_MSG", func(t *testing.T) {
		resp := GetCheckMessageByName(GCP_BACKUP_CONFIG)
		assert.Equal(t, resp, GCP_BACKUP_CONFIG_MSG)
	})

	t.Run("Get Empty MSG", func(t *testing.T) {
		resp := GetCheckMessageByName("random-string")
		assert.Equal(t, resp, "")
		assert.Empty(t, resp)
	})
}
