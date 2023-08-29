package gcputils

import (
	"context"
	"fmt"

	"cloud.google.com/go/storage"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"google.golang.org/api/option"
)

type GCPUtilsImpl struct {
}

type GCPUtils interface {
	NewSessionWithOptions(ctx context.Context, filePath string) (*storage.Client, error)
	DeleteObject(gcpClient *storage.Client, BucketName, BasePath string) (*models.Checks, error)
	ListObjectsV2(gcpClient *storage.Client, BucketName string) (*models.Checks, error)
	ListBuckets(gcpClient *storage.Client) (*models.Checks, error)
	NewUploader(ctx context.Context, bucket *storage.BucketHandle, file string) (*models.Checks, error)
}

func NewGCPUtils() GCPUtils {
	return &GCPUtilsImpl{}
}

func (au *GCPUtilsImpl) NewSessionWithOptions(ctx context.Context, filePath string) (*storage.Client, error) {
	return storage.NewClient(ctx, option.WithCredentialsFile(filePath))
}

func (au *GCPUtilsImpl) NewUploader(ctx context.Context, bucket *storage.BucketHandle, file string) (*models.Checks, error) {
	uploadObject := bucket.Object(file)

	wc := uploadObject.NewWriter(ctx)
	if _, err := fmt.Fprintf(wc, "Heute ist ein schöner Tag."); err != nil {
		return nil, err
	}
	if err := wc.Close(); err != nil {
		return nil, err
	}
	return nil, nil
}

func (au *GCPUtilsImpl) DeleteObject(gcpClient *storage.Client, BucketName, BasePath string) (*models.Checks, error) {
	return nil, nil
}

func (au *GCPUtilsImpl) ListObjectsV2(gcpClient *storage.Client, BucketName string) (*models.Checks, error) {
	return nil, nil
}

func (au *GCPUtilsImpl) ListBuckets(gcpClient *storage.Client) (*models.Checks, error) {
	return nil, nil
}
