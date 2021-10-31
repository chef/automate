package objstore

import (
	"context"
	"io"
	"io/ioutil"

	"github.com/minio/minio-go/v7"
)

type ObjectStore interface {
	PutObject(ctx context.Context, bucketName, objectName string, reader io.Reader, objectSize int64,
		opts minio.PutObjectOptions) (info minio.UploadInfo, err error)

	GetObject(ctx context.Context, bucketName, objectName string, opts minio.GetObjectOptions) (*[]byte, error)
}

type ReportManagerObjStore struct {
	ObjStoreClient *minio.Client
}

func (rmc ReportManagerObjStore) PutObject(ctx context.Context, bucketName, objectName string, reader io.Reader, objectSize int64,
	opts minio.PutObjectOptions) (info minio.UploadInfo, err error) {
	return rmc.ObjStoreClient.PutObject(ctx, bucketName, objectName, reader, objectSize, opts)
}

func (rmc ReportManagerObjStore) GetObject(ctx context.Context, bucketName, objectName string, opts minio.GetObjectOptions) (*[]byte, error) {
	obj, err := rmc.ObjStoreClient.GetObject(ctx, bucketName, objectName, opts)
	if err != nil {
		return nil, err
	}
	bytes, err := ioutil.ReadAll(obj)
	return &bytes, err
}
