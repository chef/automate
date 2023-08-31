package gcpcloudstorageservice

import (
	"context"
	"encoding/json"
	"fmt"

	"cloud.google.com/go/storage"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/sirupsen/logrus"
	"google.golang.org/api/iterator"
	"google.golang.org/api/option"
)

type GCPUtils interface {
	NewSessionWithOptions(ctx context.Context, gsa *models.GcpServiceAccount) (*storage.Client, error)
	DeleteObject(ctx context.Context, obj *storage.ObjectHandle) error
	ListObjects(ctx context.Context, bucket *storage.BucketHandle, query *storage.Query) error
	NewUploader(ctx context.Context, uploadObject *storage.ObjectHandle) error
	BucketAttributes(ctx context.Context, bucket *storage.BucketHandle) error
}

func NewGCPUtils() GCPUtils {
	return &GCPUtilsImpl{}
}

type GCPUtilsImpl struct {
}

func (au *GCPUtilsImpl) NewSessionWithOptions(ctx context.Context, gsa *models.GcpServiceAccount) (*storage.Client, error) {
	bx, err := json.Marshal(gsa)
	if err != nil {
		return nil, err
	}
	return storage.NewClient(ctx, option.WithCredentialsJSON(bx))
}

func (au *GCPUtilsImpl) NewUploader(ctx context.Context, uploadObject *storage.ObjectHandle) error {
	wc := uploadObject.NewWriter(context.Background())
	_, err := fmt.Fprintf(wc, "Today is a beautiful day!")
	if err != nil {
		return err
	}

	return nil
}

func (au *GCPUtilsImpl) DeleteObject(ctx context.Context, obj *storage.ObjectHandle) error {
	return obj.Delete(ctx)
}

func (au *GCPUtilsImpl) ListObjects(ctx context.Context, bucket *storage.BucketHandle, query *storage.Query) error {
	it := bucket.Objects(ctx, query)
	for attrs, err := it.Next(); err != iterator.Done; attrs, err = it.Next() {
		if err != nil {
			logrus.Errorf("Error listing the objects")
			return err
		}
		fmt.Println("The object", attrs.Name)
	}
	return nil
}

func (au *GCPUtilsImpl) BucketAttributes(ctx context.Context, bucket *storage.BucketHandle) error {
	if _, err := bucket.Attrs(ctx); err != nil {
		return err
	}
	return nil
}
