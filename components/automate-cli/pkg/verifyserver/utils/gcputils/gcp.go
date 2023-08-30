package gcputils

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
	NewUploader(ctx context.Context, obj *storage.ObjectHandle) (*models.Checks, error)
	BucketAttributes(ctx context.Context, bucket *storage.BucketHandle) error
}

func NewGCPUtils() GCPUtils {
	return &GCPUtilsImpl{}
}

type GCPUtilsImpl struct {
}

func (au *GCPUtilsImpl) NewSessionWithOptions(ctx context.Context, gsa *models.GcpServiceAccount) (*storage.Client, error) {
	bx, _ := json.Marshal(gsa)
	return storage.NewClient(ctx, option.WithCredentialsJSON(bx))
}

func (au *GCPUtilsImpl) NewUploader(ctx context.Context, uploadObject *storage.ObjectHandle) (*models.Checks, error) {
	wc := uploadObject.NewWriter(ctx)
	if _, err := fmt.Fprintf(wc, "Heute ist ein schöner Tag."); err != nil {
		return nil, err
	}
	if err := wc.Close(); err != nil {
		return nil, err
	}
	return nil, nil
}

func (au *GCPUtilsImpl) DeleteObject(ctx context.Context, obj *storage.ObjectHandle) error {
	return obj.Delete(ctx)
}

func (au *GCPUtilsImpl) ListObjects(ctx context.Context, bucket *storage.BucketHandle, query *storage.Query) error {
	it := bucket.Objects(ctx, query)
	for {
		attrs, err := it.Next()
		if err == iterator.Done {
			break
		}

		if err != nil {
			logrus.Errorf("Error listing the objects")
			return err
		}

		fmt.Println("The bucket", attrs.Name)
	}
	return nil
}

func (au *GCPUtilsImpl) BucketAttributes(ctx context.Context, bucket *storage.BucketHandle) error {
	_, err := bucket.Attrs(ctx)
	if err != nil {
		return err
	}
	return nil
}
