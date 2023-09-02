package gcpcloudstorageservice

import (
	"context"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

type MockGCPCloudStorageConfig struct {
	GetGCPConnectionFunc func(ctx context.Context, req *models.GCPCloudStorageConfigRequest) *models.Checks
	GetBucketAccessFunc  func(ctx context.Context, req *models.GCPCloudStorageConfigRequest) *models.Checks
}

func (msc *MockGCPCloudStorageConfig) GetGCPConnection(ctx context.Context, req *models.GCPCloudStorageConfigRequest) *models.Checks {
	return msc.GetGCPConnectionFunc(ctx, req)
}

func (msc *MockGCPCloudStorageConfig) GetBucketAccess(ctx context.Context, req *models.GCPCloudStorageConfigRequest) *models.Checks {
	return msc.GetBucketAccessFunc(ctx, req)
}
