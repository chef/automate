package constants

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestGetBastionChecks(t *testing.T) {
	resp := GetBastionChecks()
	assert.Equal(t, len(resp), 3)
	assert.Equal(t, resp[0], HARDWARE_RESOURCE_COUNT)
	assert.Equal(t, resp[1], CERTIFICATE)
	assert.Equal(t, resp[2], SSH_USER)
}

func TestGetRemoteChecks(t *testing.T) {
	resp := GetRemoteChecks()
	assert.Equal(t, len(resp), 10)
	assert.Equal(t, resp[0], SYSTEM_RESOURCES)
	assert.Equal(t, resp[1], SOFTWARE_VERSIONS)
	assert.Equal(t, resp[2], SYSTEM_USER)
	assert.Equal(t, resp[3], S3_BACKUP_CONFIG)
	assert.Equal(t, resp[4], FQDN)
	assert.Equal(t, resp[5], FIREWALL)
	assert.Equal(t, resp[6], EXTERNAL_OPENSEARCH)
	assert.Equal(t, resp[7], AWS_OPENSEARCH_S3_BUCKET_ACCESS)
	assert.Equal(t, resp[8], EXTERNAL_POSTGRESQL)
	assert.Equal(t, resp[9], NFS_BACKUP_CONFIG)
}
