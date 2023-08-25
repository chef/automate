package gcpcloudstorageservice

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

type MockGCPCloudStorageConfig struct {
	GetGCPConnectionFunc func(*models.GCPCloudStorageConfigRequest) *models.Checks
	GetBucketAccessFunc  func(*models.GCPCloudStorageConfigRequest) *models.Checks
}

func (msc *MockGCPCloudStorageConfig) GetGCPConnection(req *models.GCPCloudStorageConfigRequest) *models.Checks {
	return msc.GetGCPConnectionFunc(req)
}

func (msc *MockGCPCloudStorageConfig) GetBucketAccess(req *models.GCPCloudStorageConfigRequest) *models.Checks {
	return msc.GetBucketAccessFunc(req)
}
