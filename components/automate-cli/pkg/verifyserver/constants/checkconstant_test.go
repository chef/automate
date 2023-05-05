package constants

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestStatusService(t *testing.T) {
	resp := GetAllChecks()
	assert.Equal(t, len(resp), 13)
	assert.Equal(t, resp[0], HARDWARE_RESOURCE_COUNT)
	assert.Equal(t, resp[1], CERTIFICATE)
	assert.Equal(t, resp[2], SSH_USER)
	assert.Equal(t, resp[3], SYSTEM_RESOURCES)
	assert.Equal(t, resp[4], SOFTWARE_VERSIONS)
	assert.Equal(t, resp[5], SYSTEM_USER)
	assert.Equal(t, resp[6], S3_BACKUP_CONFIG)
	assert.Equal(t, resp[7], FQDN)
	assert.Equal(t, resp[8], FIREWALL)
	assert.Equal(t, resp[9], EXTERNAL_OPENSEARCH)
	assert.Equal(t, resp[10], AWS_OPENSEARCH_S3_BUCKET_ACCESS)
	assert.Equal(t, resp[11], EXTERNAL_POSTGRESQL)
	assert.Equal(t, resp[12], NFS_BACKUP_CONFIG)
}
