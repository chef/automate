package gcpcloudstorageservice_test

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/gcpcloudstorageservice"
	"github.com/stretchr/testify/assert"
)

func TestMockGCPCloudStorageConfig_GetGCPConnection(t *testing.T) {
	mock := &gcpcloudstorageservice.MockGCPCloudStorageConfig{
		GetGCPConnectionFunc: func(req *models.GCPCloudStorageConfigRequest) *models.Checks {
			// Return your desired mock response here
			return &models.Checks{ /* Fill with mock data */ }
		},
	}

	req := &models.GCPCloudStorageConfigRequest{ /* Fill with required data */ }
	result := mock.GetGCPConnection(req)

	// Assert the result here
	assert.NotNil(t, result)
}

func TestMockGCPCloudStorageConfig_GetBucketAccess(t *testing.T) {
	mock := &gcpcloudstorageservice.MockGCPCloudStorageConfig{
		GetBucketAccessFunc: func(req *models.GCPCloudStorageConfigRequest) *models.Checks {
			// Return your desired mock response here
			return &models.Checks{ /* Fill with mock data */ }
		},
	}

	req := &models.GCPCloudStorageConfigRequest{ /* Fill with required data */ }
	result := mock.GetBucketAccess(req)

	// Assert the result here
	assert.NotNil(t, result)
}
