package gcputils

import (
	"context"

	"cloud.google.com/go/storage"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

type MockGCPUtils struct {
	NewSessionWithOptionsFunc func(ctx context.Context, credFilePath string) (*storage.Client, error)
	ListBucketsFunc           func(gcpClient *storage.Client) (*models.Checks, error)
	DeleteObjectFunc          func(gcpClient *storage.Client, BucketName, BasePath string) (*models.Checks, error)
	ListObjectsV2Func         func(gcpClient *storage.Client, BucketName string) (*models.Checks, error)
	NewUploaderFunc           func(ctx context.Context, bucket *storage.BucketHandle, file string) (*models.Checks, error)
}

func (mau *MockGCPUtils) NewSessionWithOptions(ctx context.Context, filePath string) (*storage.Client, error) {
	return mau.NewSessionWithOptionsFunc(ctx, filePath)
}

func (mau *MockGCPUtils) ListBuckets(gcpClient *storage.Client) (*models.Checks, error) {
	return mau.ListBucketsFunc(gcpClient)
}

func (mau *MockGCPUtils) DeleteObject(gcpClient *storage.Client, BucketName, BasePath string) (*models.Checks, error) {
	return mau.DeleteObjectFunc(gcpClient, BucketName, BasePath)
}

func (mau *MockGCPUtils) ListObjectsV2(gcpClient *storage.Client, BucketName string) (*models.Checks, error) {
	return mau.ListObjectsV2Func(gcpClient, BucketName)
}

func (mau *MockGCPUtils) NewUploader(ctx context.Context, bucket *storage.BucketHandle, file string) (*models.Checks, error) {
	return mau.NewUploaderFunc(ctx, bucket, file)
}
