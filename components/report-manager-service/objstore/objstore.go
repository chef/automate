package objstore

import (
	"context"
	"io"
	"net/url"
	"time"

	"github.com/minio/minio-go/v7"
	"github.com/minio/minio-go/v7/pkg/lifecycle"
)

type ObjectStore interface {
	PutObject(ctx context.Context, bucketName, objectName string, reader io.Reader, objectSize int64,
		opts minio.PutObjectOptions) (info minio.UploadInfo, err error)

	GetObject(ctx context.Context, bucketName, objectName string, opts minio.GetObjectOptions) (io.Reader, error)

	BucketExists(ctx context.Context, bucketName string) (bool, error)

	MakeBucket(ctx context.Context, bucketName string, opts minio.MakeBucketOptions) error

	PresignedGetObject(ctx context.Context, bucketName, objectName string, expires time.Duration, reqParams url.Values) (*url.URL, error)

	StatObject(ctx context.Context, bucketName string, objectName string, opts minio.GetObjectOptions) (minio.ObjectInfo, error)

	SetBucketLifecycle(ctx context.Context, bucketName string, config *lifecycle.Configuration) error
}

type ReportManagerObjStore struct {
	ObjStoreClient *minio.Client
}

func (rmc ReportManagerObjStore) PutObject(ctx context.Context, bucketName, objectName string, reader io.Reader, objectSize int64,
	opts minio.PutObjectOptions) (info minio.UploadInfo, err error) {
	return rmc.ObjStoreClient.PutObject(ctx, bucketName, objectName, reader, objectSize, opts)
}

func (rmc ReportManagerObjStore) GetObject(ctx context.Context, bucketName, objectName string, opts minio.GetObjectOptions) (io.Reader, error) {
	return rmc.ObjStoreClient.GetObject(ctx, bucketName, objectName, opts)
}

func (rmc ReportManagerObjStore) BucketExists(ctx context.Context, bucketName string) (bool, error) {
	return rmc.ObjStoreClient.BucketExists(ctx, bucketName)
}

func (rmc ReportManagerObjStore) MakeBucket(ctx context.Context, bucketName string, opts minio.MakeBucketOptions) (err error) {
	return rmc.ObjStoreClient.MakeBucket(ctx, bucketName, opts)
}

func (rmc ReportManagerObjStore) PresignedGetObject(ctx context.Context, bucketName string, objectName string, expires time.Duration, reqParams url.Values) (u *url.URL, err error) {
	return rmc.ObjStoreClient.PresignedGetObject(ctx, bucketName, objectName, expires, reqParams)
}

func (rmc ReportManagerObjStore) StatObject(ctx context.Context, bucketName string, objectName string, opts minio.GetObjectOptions) (minio.ObjectInfo, error) {
	return rmc.ObjStoreClient.StatObject(ctx, bucketName, objectName, opts)
}

func (rmc ReportManagerObjStore) SetBucketLifecycle(ctx context.Context, bucketName string, config *lifecycle.Configuration) error {
	return rmc.ObjStoreClient.SetBucketLifecycle(ctx, bucketName, config)
}
