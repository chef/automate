package gcputils

import (
	"context"

	"cloud.google.com/go/storage"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

type MockGCPUtils struct {
	NewSessionWithOptionsFunc func(ctx context.Context, gsa *models.GcpServiceAccount) (*storage.Client, error)
	ListBucketsFunc           func(gcpClient *storage.Client) (*models.Checks, error)
	DeleteObjectFunc          func(ctx context.Context, uploadObject *storage.ObjectHandle) error
	ListObjectsFunc           func(ctx context.Context, bucket *storage.BucketHandle, query *storage.Query) error
	NewUploaderFunc           func(ctx context.Context, uploadObject *storage.ObjectHandle) (*models.Checks, error)
	BucketAttributesFunc      func(ctx context.Context, bucket *storage.BucketHandle) error
}

func (mau *MockGCPUtils) NewSessionWithOptions(ctx context.Context, gsa *models.GcpServiceAccount) (*storage.Client, error) {
	return mau.NewSessionWithOptionsFunc(ctx, gsa)
}

func (mau *MockGCPUtils) ListBuckets(gcpClient *storage.Client) (*models.Checks, error) {
	return mau.ListBucketsFunc(gcpClient)
}

func (mau *MockGCPUtils) DeleteObject(ctx context.Context, uploadObject *storage.ObjectHandle) error {
	return mau.DeleteObjectFunc(ctx, uploadObject)
}

func (mau *MockGCPUtils) ListObjects(ctx context.Context, bucket *storage.BucketHandle, query *storage.Query) error {
	return mau.ListObjectsFunc(ctx, bucket, query)
}

func (mau *MockGCPUtils) NewUploader(ctx context.Context, uploadObject *storage.ObjectHandle) (*models.Checks, error) {
	return mau.NewUploaderFunc(ctx, uploadObject)
}

func (mau *MockGCPUtils) BucketAttributes(ctx context.Context, bucket *storage.BucketHandle) error {
	return mau.BucketAttributesFunc(ctx, bucket)
}
